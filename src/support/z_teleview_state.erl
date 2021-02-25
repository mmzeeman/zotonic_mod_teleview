%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2021 Maas-Maarten Zeeman
%% @doc TeleView State.

%% Copyright 2019-2021 Maas-Maarten Zeeman 
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_teleview_state).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-behaviour(gen_server).

% api
-export([
    start_link/4,
    start_renderer/3
]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(INTERVAL_MSEC, 30000).

-define(RENDERER_WARN_TIME1, 300). % 5 min
-define(RENDERER_WARN_TIME2, 360). % 6 min
-define(RENDERER_EXPIRE_TIME, 420). % 7 min

-define(MAX_NO_RENDERERS_COUNT, 2).

-record(state, {
          id,

          args, 

          teleview_supervisor = undefined,

          renderers_supervisor = undefined,
          renderers = #{}, 

          no_renderers_count = 0,

          context
         }).

% @doc Start the state process.
start_link(Id, Supervisor, Args, Context) ->
    gen_server:start_link(
      {via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE, [Id, Supervisor, Args, Context], []).


% @doc Start a renderer.
start_renderer(TeleviewId, Args, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, TeleviewId}, Context}},
                    {start_renderer, Args, Context}).

%%
%% gen_server callbacks.
%%

init([Id, Supervisor, #{ <<"topic">> := Topic }=Args, Context]) ->
    process_flag(trap_exit, true),

    self() ! get_renderers_sup_pid,

    %% Subscribe to event topic.
    case z_mqtt:subscribe(Topic, Context) of
        ok ->
            ok;
        {error, _}=Error ->
            % log warning
            z:warning("Teleview could not subscribe to topic: ~p, reason: ~p",
                      [Topic, Error],
                      [{module, ?MODULE}, {line, ?LINE}],
                      Context),
            ok
    end,

    trigger_check(),
        
    {ok, #state{id=Id, teleview_supervisor=Supervisor, args=Args, context=Context}}.

handle_call({start_renderer, Args, RenderContext}, _From,
            #state{renderers_supervisor=RenderersSup,
                   args=TeleviewArgs}=State) when is_pid(RenderersSup) ->

    RenderArgs = maps:merge(Args, TeleviewArgs),
    RendererId = erlang:phash2(RenderArgs),

    PublishTopic = z_mqtt:flatten_topic([model, teleview, event, State#state.id, RendererId]),

    case supervisor:start_child(RenderersSup, [RendererId, PublishTopic, RenderArgs,
                                               z_context:prune_for_async(RenderContext)]) of
        {ok, Pid} ->
            MonitorRef = erlang:monitor(process, Pid),
            Renderers1 = maps:put(Pid, #{renderer_id => RendererId,
                                         last_check => z_utils:now(), 
                                         monitor_ref => MonitorRef}, State#state.renderers),
            RendererState = #{publish_topic => PublishTopic},
            {reply, {ok, RendererState}, State#state{no_renderers_count=0, renderers=Renderers1}};
        {error, {already_started, _Pid}} ->
            RendererState = z_teleview_differ:state(State#state.id, RendererId, RenderContext),
            {reply, {ok, RendererState}, State#state{no_renderers_count=0}};
        {error, Error} ->
            {reply, {error, {could_not_start, Error}}, State}
    end;

handle_call({start_renderer, Args, RenderContext}, _From, #state{args=TeleviewArgs}=State) ->
    {stop, no_renderer_supervisor, State};

handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(check, #state{renderers=Renderers, no_renderers_count=N}=State)
  when map_size(Renderers) =:= 0 andalso N > ?MAX_NO_RENDERERS_COUNT ->
    ?DEBUG({no_renderers_count, N}),
    publish_event(State#state.id, stopped, State#state.context),
    exit(State#state.teleview_supervisor, normal),
    {stop, normal, State};
handle_info(check, #state{renderers=Renderers, renderers_supervisor=Sup}=State) ->
    Now = z_utils:now(),
    maps:map(fun(Pid, #{last_check := LastCheck}=RendererInfo) ->
                     case Now of
                         N when N < (LastCheck + ?RENDERER_WARN_TIME1) ->
                             ok;
                         N when N < (LastCheck + ?RENDERER_WARN_TIME2) ->
                             publish_event(State#state.id,
                                           maps:get(renderer_id, RendererInfo), still_watching, State#state.context);
                         N when N < (LastCheck + ?RENDERER_EXPIRE_TIME) ->
                             publish_event(State#state.id,
                                           maps:get(renderer_id, RendererInfo), still_watching, State#state.context);
                         _ ->
                             ?DEBUG({expire, RendererInfo}),

                             case supervisor:terminate_child(Sup, Pid) of
                                 ok ->
                                     publish_event(State#state.id, maps:get(renderer_id, RendererInfo), stopped, State#state.context),
                                     supervisor:delete_child(Sup, Pid);
                                 {error, Reason} ->
                                     ?DEBUG({could_not_terminate, Reason})
                             end,

                             ok
                     end
             end,
             Renderers),

    trigger_check(),

    case map_size(Renderers) of
        0 ->
            {noreply, State#state{no_renderers_count=State#state.no_renderers_count+1}};
        _ ->
            {noreply, State}
    end;

handle_info(get_renderers_sup_pid, State) ->
    case get_renderers_sup_pid(State#state.teleview_supervisor) of
        undefined ->
            {stop, {error, no_renderers_supervisor}, State};
        Pid when is_pid(Pid) ->
            {noreply, State#state{renderers_supervisor=Pid}}
    end; 

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, #state{renderers=Renderers}=State) ->
    %% A renderer died.
    %%
    ?DEBUG({handle_renderer_down, Pid}),
    Renderers1 = maps:remove(Pid, Renderers),
    {noreply, State#state{renderers=Renderers1}};

handle_info({mqtt_msg, Msg}, State) ->
    %% Trigger a render on all renderers.
    trigger_render(State#state.id,
                   State#state.renderers,
                   #{mqtt_msg => maps:without([publisher_context], Msg)},
                   State#state.context),

    {noreply, State};
handle_info(Info, State) ->
    ?DEBUG(Info),
    {noreply, State}.

terminate(Reason, _State) ->
    ?DEBUG({terminate, Reason}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Helpers
%%


trigger_check() ->
    erlang:send_after(?INTERVAL_MSEC, self(), check).

trigger_render(TeleviewId, Renderers, Args, Context) ->
    maps:map(fun(_RendererSupPid, #{renderer_id := RendererId}) ->
                     z_teleview_render:render(TeleviewId, RendererId, Args, Context)
             end,
             Renderers),
    ok.


publish_event(TeleviewId, Event, Context) ->
    z_mqtt:publish([model, teleview, TeleviewId, Event], #{}, z_acl:sudo(Context)).

publish_event(TeleviewId, RendererId, Event, Context) ->
    z_mqtt:publish([model, teleview, TeleviewId, Event, RendererId], #{}, z_acl:sudo(Context)).

% Get renderers supervisor pid from the supervisor.
get_renderers_sup_pid(SupervisorPid) when is_pid(SupervisorPid) ->
    get_renderers_sup_pid(supervisor:which_children(SupervisorPid));
get_renderers_sup_pid([]) ->
    undefined;
get_renderers_sup_pid([{z_teleview_renderers_sup, Pid, _, _}|_Rest]) ->
    Pid;
get_renderers_sup_pid([_Child|Rest]) ->
    get_renderers_sup_pid(Rest).


    
