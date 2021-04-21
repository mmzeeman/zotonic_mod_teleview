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
    start_renderer/3,

    keep_alive/3
]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(INTERVAL_MSEC, 30000).

-define(RENDERER_WARN_TIME, 60). % 5 min
-define(RENDERER_EXPIRE_TIME, 420). % 7 min

-define(MAX_NO_RENDERERS_COUNT, 2).

-record(state, {
          id,

          args, 

          teleview_supervisor = undefined,  %% The supervisor of the teleview

          renderers_supervisor = undefined, %% The supervisor of all renderers
          renderers = #{}, 

          no_renderers_count = 0, %% Counter of how many times we saw that this
                                  %% teleview has no renderers.

          context
         }).

% @doc Start the state process.
start_link(Id, Supervisor, Args, Context) ->
    StateContext = state_context(Args, Context),
    gen_server:start_link(
      {via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE, [Id, Supervisor, Args, StateContext], []).


% @doc Start a renderer.
start_renderer(TeleviewId, VaryArgs, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, TeleviewId}, Context}},
                    {start_renderer, VaryArgs}).

% @doc Tell the teleview state process to keep the renderer alive
keep_alive(TeleviewId, RendererId, Context) ->
    gen_server:cast({via, z_proc, {{?MODULE, TeleviewId}, Context}}, {keep_alive, RendererId}).

%%
%% gen_server callbacks.
%%

init([Id, Supervisor, #{ topics := Topics }=Args, Context]) ->
    process_flag(trap_exit, true),

    self() ! get_renderers_sup_pid,

    ok = subscribe(Topics, Context),

    m_teleview:publish_event(started, Id, #{ }, Context),

    trigger_check(),
        
    {ok, #state{id=Id, teleview_supervisor=Supervisor, args=Args, context=Context}}.

handle_call({start_renderer, VaryArgs}, _From,
            #state{renderers_supervisor=RenderersSup,
                   args=TeleviewArgs}=State) when is_pid(RenderersSup) ->

    RenderArgs = maps:merge(TeleviewArgs, VaryArgs),
    RendererId = erlang:phash2(RenderArgs),

    case supervisor:start_child(RenderersSup, [RendererId, RenderArgs, State#state.context]) of 
        {ok, Pid} ->
            MonitorRef = erlang:monitor(process, Pid),
            Renderers1 = maps:put(Pid, #{renderer_id => RendererId,
                                         last_check => z_utils:now(), 
                                         monitor_ref => MonitorRef}, State#state.renderers),
            RendererState = #{teleview_id => State#state.id, renderer_id  => RendererId},
            {reply, {ok, RendererState}, State#state{no_renderers_count=0, renderers=Renderers1}};
        {error, {already_started, _Pid}} ->
            RendererState = z_teleview_differ:state(State#state.id, RendererId, State#state.context),
            {reply, {ok, RendererState}, State#state{no_renderers_count=0}};
        {error, Error} ->
            {reply, {error, {could_not_start, Error}}, State}
    end;

handle_call({start_renderer, _Args, _RenderContext}, _From, #state{args=_TeleviewArgs}=State) ->
    {stop, no_renderer_supervisor, State};

handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast({keep_alive, RendererId}, State) ->
    Renderers1 = maps:map(fun(_Pid, Info) ->
                                  case maps:get(renderer_id, Info) of
                                      RendererId ->
                                          Info#{last_check => z_utils:now()};
                                      _ ->
                                          Info
                                  end
                          end,
                          State#state.renderers),
    {noreply, State#state{renderers=Renderers1}};

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(check, #state{renderers=Renderers, no_renderers_count=N}=State)
  when map_size(Renderers) =:= 0 andalso N > ?MAX_NO_RENDERERS_COUNT ->
    mod_teleview:stop_teleview(State#state.id, State#state.context),
    {noreply, State};
handle_info(check, #state{renderers=Renderers, renderers_supervisor=Sup}=State) ->
    Now = z_utils:now(),

    maps:map(fun(Pid, #{last_check := LastCheck}=RendererInfo) ->
                     case Now of
                         N when N < (LastCheck + ?RENDERER_WARN_TIME) ->
                             ok;
                         N when N < (LastCheck + ?RENDERER_EXPIRE_TIME) ->
                             m_teleview:publish_event(still_watching,
                                                      State#state.id, maps:get(renderer_id, RendererInfo),
                                                      #{},
                                                      State#state.context);
                         _ ->
                             case supervisor:terminate_child(Sup, Pid) of
                                 ok ->
                                     m_teleview:publish_event(stopped,
                                                              State#state.id, maps:get(renderer_id, RendererInfo),
                                                              #{},
                                                              State#state.context),
                                     supervisor:delete_child(Sup, Pid);
                                 {error, Reason} ->
                                     ?DEBUG({could_not_terminate, Reason})
                             end
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
            link(Pid),
            {noreply, State#state{renderers_supervisor=Pid}}
    end; 

handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, #state{renderers=Renderers}=State) ->
    %% A renderer died.
    %%
    case maps:get(Pid, Renderers, undefined) of
        undefined ->
            {noreply, State};
        RendererInfo ->
            RendererId = maps:get(renderer_id, RendererInfo),
            m_teleview:publish_event(down, State#state.id, RendererId, #{ reason => Reason }, State#state.context),
            Renderers1 = maps:remove(Pid, Renderers),
            {noreply, State#state{renderers=Renderers1}}
    end;

handle_info({mqtt_msg, Msg}, State) ->
    %% Trigger a render on all renderers.
    Args1 = case z_notifier:first({teleview_render, State#state.id, Msg, State#state.args}, State#state.context) of
                undefined -> State#state.args;
                NewArgs when is_map(NewArgs) -> NewArgs
            end,

    trigger_render(State#state.id,
                   State#state.renderers,
                   Args1,
                   State#state.context),

    {noreply, State#state{args=Args1}};
handle_info(Info, State) ->
    ?DEBUG(Info),
    {noreply, State}.

terminate(Reason, State) ->
    m_teleview:publish_event(stopped, State#state.id, #{ reason => Reason }, State#state.context),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Helpers
%%

state_context(Args, Context) ->
    case z_notifier:first({teleview_state_init, Args}, Context) of
        undefined ->
            z_acl:anondo(Context);
        #context{} = NewContext ->
            NewContext
    end.

subscribe([], _Context) ->
    ok;
subscribe([Topic|Rest], Context) ->
    %% Subscribe to event topic.
    case z_mqtt:subscribe(Topic, Context) of
        ok ->
            subscribe(Rest, Context);
        {error, _}=Error ->
            % log warning
            z:warning("Teleview could not subscribe to topic: ~p, reason: ~p",
                      [Topic, Error],
                      [{module, ?MODULE}, {line, ?LINE}],
                      Context),
            Error
    end.


trigger_check() ->
    erlang:send_after(?INTERVAL_MSEC, self(), check).

trigger_render(TeleviewId, Renderers, Args, Context) ->
    maps:map(fun(_RendererSupPid, #{renderer_id := RendererId}) ->
                     z_teleview_render:render(TeleviewId, RendererId, Args, Context)
             end,
             Renderers),
    ok.


% Get renderers supervisor pid from the supervisor.
get_renderers_sup_pid(SupervisorPid) when is_pid(SupervisorPid) ->
    get_renderers_sup_pid(supervisor:which_children(SupervisorPid));
get_renderers_sup_pid([]) ->
    undefined;
get_renderers_sup_pid([{z_teleview_renderers_sup, Pid, _, _}|_Rest]) ->
    Pid;
get_renderers_sup_pid([_Child|Rest]) ->
    get_renderers_sup_pid(Rest).


