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

% api
-export([
    start_link/3,
    render/2,
    render/3
]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {
          id,

          template,
          context
         }).

%%
%% api
%%

start_link(Id, Args, Context) ->
    gen_server:start_link({via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE, [Id, Args, Context], []).


% render without arguments
render(Id, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, Id}, Context}}, render).

% render with arguments
render(Id, Args, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, Id}, Context}}, {render, Args}).

%%
%% gen_server callbacks.
%%

init([Id, #{template := Template}, Context]) ->
    {ok, #state{id=Id, template=Template, context=Context}}.

handle_call(render, _From, State) ->
    {ok, State1} = render_and_diff([], State),
    {reply, ok, State1};
handle_call({render, Vars}, _From, State) ->
    {ok, State1} = render_and_diff(Vars, State),
    {reply, ok, State1};
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

%%
%% Helpers
%%

% Render the template with the supplied vars and send the result to the differ.
render_and_diff(Vars, #state{template=Template, context=Context}=State) ->
    {IOList, Context1} = z_template:render_to_iolist(Template, Vars, Context),
    NewFrame = z_convert:to_binary(IOList),

    State1 = State#state{context=Context1},

    case z_teleview_differ:new_frame(NewFrame, Context) of
        busy ->
            %% The differ could not handle the new frame. It will be dropped.
            lager:warning("Differ ~p is busy. Could not update view", [State#state.id]),
            {ok, State1};
        ok ->
            {ok, State1}
    end.
 
