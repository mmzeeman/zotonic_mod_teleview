%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman
%% @doc TeleView Render.

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

%%
%% Responsible for rendering updated views... Sends the result to the differ
%%

-module(z_teleview_render).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
          id,
          context
         }).

start_link(Id, Args, Context) ->
    gen_server:start_link({via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE, [Id, Args, Context], []).

-include_lib("zotonic_core/include/zotonic.hrl").

%%
%% gen_server callbacks.
%%

init([Id, _Args, Context]) ->
    {ok, #state{id=Id, context=Context}}.

handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(Info, State) ->
    ?DEBUG(Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

