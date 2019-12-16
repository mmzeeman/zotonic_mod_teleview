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
    start_link/3,
    get_topics/2
]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {
          id,

          renderers_supervisor = undefined,
          renderers = #{}, 

          context
         }).


start_link(Id, Supervisor, Context) ->
    gen_server:start_link(
      {via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE, [Id, Supervisor, Context], []).


% @doc Return the render topic which can be used for this context.
get_topics(Id, Context) ->
    case z_proc:whereis({?MODULE, Id}, Context) of
        Pid when is_pid(Pid) ->
            gen_server:call(get_topics, Pid);
        undefined ->
            %% This is an unknown renderer, there is no event topic.
            undefined
    end.

%%
%% gen_server callbacks.
%%

init([Id, Supervisor, Context]) ->
    %% Start the renderers supervisor.
    self() ! {start_renderers_supervisor, Supervisor, Id, Context},
    {ok, #state{id=Id, context=Context}}.

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
handle_info(Info, State) ->
    ?DEBUG(Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


