%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2024 Maas-Maarten Zeeman
%% @doc TeleView State.

%% Copyright 2019-2024 Maas-Maarten Zeeman 
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     https://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_teleview_state).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-behaviour(gen_server).

-include_lib("include/teleview.hrl").

% api
-export([
    start_link/4,
    start_renderer/3,

    init_table/1,
    store_current_frame/6,

    get_current_frame/3, get_current_frame/4, get_current_frame/5,

    model_post/4,

    store_keyframe/6,
    get_keyframe/3, 

    delete_frames/3
]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(INTERVAL_MSEC, 30000).
-define(MAX_NO_RENDERERS_COUNT, 2).

-record(state, {
    id :: mod_teleview:id(),

    args :: term(),

    teleview_supervisor = undefined,  %% The supervisor of the teleview
    renderers_supervisor = undefined, %% The supervisor of all renderers

    no_renderers_count = 0 :: non_neg_integer(), %% Counter of how many times we saw that this
                                                 %% teleview has no renderers.
    context :: z:context()
}).

% @doc Start the state process.
start_link(Id, Supervisor, Args, Context) ->
    {Args1, Context1} = state_context(Args, Context),
    gen_server:start_link(
      {via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE, [Id, Supervisor, Args1, Context1], []).


% @doc Start a renderer.
-spec start_renderer(integer(), map(), z:context()) -> {ok, integer()} | {error, _}.
start_renderer(TeleviewId, VaryArgs, Context) ->
    %% Generate a stable renderer id from the id of the teleview and the vary args of the renderer
    RendererId = mod_teleview:renderer_id(TeleviewId, VaryArgs),

    case is_renderer_already_started(TeleviewId, RendererId, Context) of
        true ->
            {ok, RendererId};
        false ->
            %% Ensure access from this session to the teleview.
            gen_server:call({via, z_proc, {{?MODULE, TeleviewId}, Context}},
                            {start_renderer, VaryArgs, z_context:prune_for_scomp(Context)})
    end.

model_post(TeleviewId, Path, Msg, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, TeleviewId}, Context}},
                    {model_post, Path, Msg, z_context:prune_for_scomp(Context)}).


% @doc Return true when the renderer is already started.
is_renderer_already_started(TeleviewId, RendererId, Context) ->
    z_teleview_renderer:is_already_started(TeleviewId, RendererId, Context).


%%
%% Teleview Ets State. This table is shared by all televiews. It contains the frames
%% of the different renderers.
%%

init_table(Context) ->
    Table = table_name(Context),
    ets:new(Table, [named_table, set, {keypos, 1},
                    public,
                    {write_concurrency, true},
                    {read_concurrency, true}]).


table_name(Context) ->
    z_utils:name_for_site(?MODULE, Context).


% @doc Store the current frame of a renderer.
store_current_frame(SpawnTimestamp, TeleviewId, RendererId, Frame, Sn, Context) ->
    Table = table_name(Context),
    ets:insert(Table, {{current_frame, TeleviewId, RendererId}, SpawnTimestamp, Frame, Sn}).

% @doc Get the current frame of the specified teleview and rederer.
get_current_frame(TeleviewId, RendererId, Context) ->
    Table = table_name(Context),
    case ets:lookup(Table, {current_frame, TeleviewId, RendererId}) of
        [] ->
            {error, enoent};
        [{_Key, SpawnTimestamp, Frame, Sn}] ->
            #{ state => ok, sts => SpawnTimestamp, current_frame => Frame, current_frame_sn => Sn }
    end.

% @doc Get the current frame of the specified teleview and rederer. By passing the pickle the
% server (re)-starts the renderer when it is not running.
get_current_frame(TeleviewId, RendererId, #{ <<"pickle">> := Pickle }=R, Context) ->
    case get_current_frame(TeleviewId, RendererId, Context) of
        {error, enoent} ->
            #{ teleview_id := TeleviewId, renderer_id := RendererId,
               teleview_args := TeleviewArgs, renderer_args := RendererArgs } = z_utils:depickle(Pickle, Context),
            get_current_frame(TeleviewId, RendererId, TeleviewArgs, RendererArgs, Context);
        #{ current_frame_sn := SN, sts := STS}=Map ->
            %% Strip the current frame when we have a matching current frame number.
            RFrameSN = maps:get(<<"current_frame_sn">>, R, undefined),
            RSTN = maps:get(<<"sts">>, R, undefined),
            case RFrameSN == SN andalso RSTN == STS of
                true ->
                    maps:without([current_frame], Map);
                false ->
                    Map
            end
    end.

get_current_frame(TeleviewId, RendererId, TeleviewArgs, RendererArgs, Context) ->
    case get_current_frame(TeleviewId, RendererId, Context) of
        Map when is_map(Map) ->
            Map;
        {error, enoent} ->
            case scomp_teleview_teleview:ensure_renderer(TeleviewArgs, RendererArgs, Context) of
                {ok, TeleviewId, RendererId} ->
                    get_current_frame(TeleviewId, RendererId, Context);
                {error, Error} ->
                    ?LOG_WARNING(#{ text => "Could not restart renderer",
                                    error => Error }),
                    {error, restart_problem}
            end
    end.

% @doc Store the keyframe of a renderer.
store_keyframe(SpawnTimestamp, TeleviewId, RendererId, Frame, Sn, Context) ->
    Table = table_name(Context),
    ets:insert(Table, {{keyframe, TeleviewId, RendererId}, SpawnTimestamp, Frame, Sn}).

get_keyframe(TeleviewId, RendererId, Context) ->
    Table = table_name(Context),
    case ets:lookup(Table, {keyframe, TeleviewId, RendererId}) of
        [] ->
            undefined;
        [{_Key, SpawnTimestamp, Frame, Sn}] ->
            #{ sts => SpawnTimestamp,
               frame => Frame,
               keyframe_sn => Sn }
    end.

% @doc Remove the current and keyframe of a renderer.
delete_frames(TeleviewId, RendererId, Context) ->
    Table = table_name(Context),
    ets:delete(Table, {current_frame, TeleviewId, RendererId}),
    ets:delete(Table, {keyframe, TeleviewId, RendererId}),
    ok.

%%
%% gen_server callbacks.
%%

init([Id, Supervisor, #{ topics := Topics }=Args, Context]) ->
    process_flag(trap_exit, true),

    self() ! get_renderers_sup_pid,

    ok = subscribe(Topics, Context),
    ok = setup_tick(Args),

    trigger_check(),
        
    {ok, #state{id=Id, teleview_supervisor=Supervisor, args=Args, context=Context}}.

handle_call({start_renderer, VaryArgs, Context}, _From,
            #state{renderers_supervisor=RenderersSup,
                   args=TeleviewArgs}=State) when is_pid(RenderersSup) ->

    %% Generate a stable renderer id from the id of the teleview and the vary args of the renderer
    RendererId = mod_teleview:renderer_id(State#state.id, VaryArgs),
    RenderArgs = maps:merge(TeleviewArgs, VaryArgs),

    case supervisor:start_child(RenderersSup, [RendererId, RenderArgs, Context]) of 
        {ok, _Pid} ->
            {reply, {ok, RendererId}, State#state{no_renderers_count=0}};
        {error, {already_started, _Pid}} ->
            {reply, {ok, RendererId}, State#state{no_renderers_count=0}};
        {error, Error} ->
            {reply, {error, {could_not_start, Error}}, State}
    end;

handle_call({start_renderer, _Args, _RenderContext}, _From, #state{args=_TeleviewArgs}=State) ->
    {stop, no_renderer_supervisor, State};

handle_call({model_post, Path, Msg, CallContext}, _From, State) ->
    case z_notifier:first(#teleview_model_post{ id = State#state.id, path = Path, msg = Msg, args = State#state.args}, CallContext) of
        undefined ->
            {reply, ok, State};
        {ok, Args1} ->
            State1 = State#state{ args = Args1 },
            ?DEBUG(render),
            handle_render(#{ state_update => true }, State1),
            {reply, ok, State1}
    end;

handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(check, #state{renderers_supervisor=Sup}=State) ->
    trigger_check(),

    {active, Active} = proplists:lookup(active, supervisor:count_children(Sup)),

    case Active of
        0 ->
            case State#state.no_renderers_count+1 of
                N when N > ?MAX_NO_RENDERERS_COUNT ->
                    F = fun() ->
                                ok = mod_teleview:stop_teleview(State#state.id, State#state.context)
                        end,
                    spawn(F),
                    {stop, normal, State};
                Count ->
                    {noreply, State#state{no_renderers_count=Count}}
            end;
        N when N > 0 ->
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

handle_info({tick, Interval}, State) ->
    try
        handle_render(#{ tick => Interval }, State)
    after
        trigger_tick(Interval)
    end;

handle_info({mqtt_msg, Msg}, State) ->
    handle_render(Msg, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Helpers
%%

handle_render(Msg, State) ->
    %% Trigger a render on all renderers.
    Args1 = case z_notifier:first(#teleview_render{ id = State#state.id,
                                                    msg = Msg,
                                                    args = State#state.args}, State#state.context) of
                undefined -> State#state.args;
                NewArgs when is_map(NewArgs) -> NewArgs
            end,
    trigger_render(supervisor:which_children(State#state.renderers_supervisor), Args1, State#state.context),
    {noreply, State#state{args=Args1}}.

state_context(Args, Context) ->
    case z_notifier:first(#teleview_state_init{args = Args}, Context) of
        undefined ->
            {Args, z_acl:anondo(Context)};
        #context{}=Context1 ->
            {Args, Context1};
        {ok, #context{}=Context1} ->
            {Args, Context1};
        {ok, #{}=Args1, #context{}=Context1} ->
            {Args1, Context1}
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

            %% Keep on trying to subscribe to the other topics
            subscribe(Rest, Context)
    end.

setup_tick(#{ tick := Tick }) ->
    trigger_tick(Tick);
setup_tick(#{ }) ->
    ok.

trigger_tick(TickInterval) ->
    _ = erlang:send_after(TickInterval, self(), {tick, TickInterval}),
    ok.

trigger_check() ->
    erlang:send_after(?INTERVAL_MSEC, self(), check).

trigger_render(Renderers, Args, Context) ->
    lists:foreach(fun({_, Pid, _, _}) when is_pid(Pid) ->
                          z_teleview_renderer:render(Pid, Args, Context);
                     ({_,_,_,_}) ->
                          skip
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

