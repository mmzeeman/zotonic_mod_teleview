%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman
%% @doc TeleView State.

%% Copyright 2019 Maas-Maarten Zeeman 
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
    get_topics/2,

    start_renderer/3
]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {
          id,

          args, 

          renderers_supervisor = undefined,
          renderers = #{}, 

          context
         }).

% @doc Start the state process.
start_link(Id, Supervisor, Args, Context) ->
    gen_server:start_link(
      {via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE, [Id, Supervisor, Args, Context], []).


% @doc Return the render topic which can be used for this context.
get_topics(Id, Context) ->
    case z_proc:whereis({?MODULE, Id}, Context) of
        Pid when is_pid(Pid) ->
            gen_server:call(get_topics, Pid);
        undefined ->
            %% This is an unknown renderer, there is no event topic.
            undefined
    end.

% @doc Start a renderer.
start_renderer(TeleviewId, Args, Context) ->
    RenderId = erlang:phash2(Args),
    start_renderer(TeleviewId, RenderId, Args, Context).


start_renderer(TeleviewId, RendererId, Args, Context) ->
    case gen_server:call({via, z_proc, {{?MODULE, TeleviewId}, Context}},
                         {start_renderer, RendererId, Args, Context}) of
        {ok, _Pid} ->
            {ok, RendererId};
        {error, {already_started, _Pid}} ->
            {ok, RendererId};
        {error, _}=Error ->
            Error
    end.


%%
%% gen_server callbacks.
%%

init([Id, Supervisor, #{ <<"topic">> := Topic }=Args, Context]) ->
    %% Start the renderers supervisor.
    self() ! {start_renderers_supervisor, Supervisor, Id, Context},

    %% Subscribe to event topic.
    case z_mqtt:subscribe(Topic, Context) of
        ok -> ok;
        {error, _}=Error ->
            % log warning
            z:warning("Teleview could not subscribe to topic: ~p, reason: ~p",
                      [Topic, Error],
                      [{module, ?MODULE}, {line, ?LINE}],
                      Context),
            ok
    end,
        
    {ok, #state{id=Id, args=Args, context=Context}}.

handle_call({start_renderer, RendererId, Args, RenderContext}, _From,
            #state{renderers_supervisor=RenderersSup,
                   args=TeleviewArgs}=State) when is_pid(RenderersSup) ->

    Args1 = case maps:is_key(<<"template">>, Args) of
                true -> Args;
                false -> Args#{template => maps:get(<<"template">>, TeleviewArgs)}
            end,

    PublishTopic = [model, teleview, event, State#state.id, RendererId],

    ?DEBUG(PublishTopic),

    case supervisor:start_child(RenderersSup, [RendererId, PublishTopic, Args1,
                                               z_context:prune_for_async(RenderContext)]) of
                    {ok, Pid} ->
                        _MonitorRef = erlang:monitor(process, Pid),
                        Renderers1 = maps:put(RendererId, Pid, State#state.renderers),
                        {reply, {ok, Pid}, State#state{renderers=Renderers1}};
                    {error, {already_started, Pid}} ->
                        {reply, {ok, Pid}, State};
                    {error, Error} ->
                        {reply, {error, {could_not_start, Error}}, State}
    end;
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info({start_renderers_supervisor, Sup, Id, Context}, State) ->
    MFA = {z_teleview_renderers_sup, start_link, [Id, Context]},

    RenderersSpec = #{id => z_teleview_renderers_sup,
                      start => MFA, 
                      restart => transient, 
                      shutdown => infinity,
                      type => supervisor,
                      modules => [z_teleview_renderers_sup,
                                  z_teleview_renderer_sup,
                                  z_teleview_differ,
                                  z_teleview_render]},

    {ok, Pid} = supervisor:start_child(Sup, RenderersSpec),
    link(Pid),

    {noreply, State#state{renderers_supervisor=Pid}};
handle_info({mqtt_msg, Msg}, State) ->
    ?DEBUG(Msg),
    %% Received a message from the mqtt subscription

    %% Trigger a render on all renderers.
    trigger_render(State#state.id, Msg, State#state.renderers, State#state.context),

    {noreply, State};
handle_info(Info, State) ->
    ?DEBUG(Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Helpers
%%

trigger_render(TeleviewId, Msg, Renderers, Context) ->
    ?DEBUG({trigger_render, Renderers}),

    maps:map(fun(RendererId, _RendererSup) ->
                     %% [TODO] What should be in the message?
                     z_teleview_render:render(TeleviewId, RendererId, Msg, Context)
             end,
             Renderers),


    ok.


