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
    start_link/5,
    render/3,
    render/4
]).


% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {
          template,
          args,
          render_context,

          differ_pid
}).

%%
%% api
%%

start_link(TeleviewId, RendererId, SupervisorPid, Args, Context) ->
    gen_server:start_link({via, z_proc, {{?MODULE, TeleviewId, RendererId}, Context}},
                          ?MODULE,
                          [SupervisorPid, TeleviewId, RendererId, Args, Context], []).


% render without arguments
render(TeleviewId, RenderId, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, TeleviewId, RenderId}, Context}}, render).

% render with arguments
render(TeleviewId, RenderId, Args, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, TeleviewId, RenderId}, Context}}, {render, Args}).

%%
%% gen_server callbacks.
%%

init([Supervisor, TeleviewId, RendererId, #{<<"template">> := Template}=Args, Context]) ->
    %% Get the differ pid from the supervisor. Note that this can't be done in the
    %% init, otherwise the supervisor will deadlock causing a timeout.
    self() ! {get_differ_pid, Supervisor},

    %% Add the teleview_id and the renderer_id to the arguments.
    Args1 = Args#{teleview_id => TeleviewId,
                  renderer_id => RendererId},

    {ok, #state{template=Template,
                args=Args1,
                render_context=Context}}.

handle_call(render, _From, State) ->
    {ok, State1} = render_and_diff(#{}, State),
    {reply, ok, State1};
handle_call({render, Args}, _From, State) ->
    {ok, State1} = render_and_diff(Args, State),
    {reply, ok, State1};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info({get_differ_pid, Supervisor}, State) ->
    case get_differ_pid(Supervisor) of
        undefined ->
            {noreply, State}; 
        Pid when is_pid(Pid) ->
            {noreply, State#state{differ_pid = Pid}}
    end;
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
render_and_diff(Args, #state{template=Template, args=RenderArgs, render_context=Context, differ_pid=DifferPid}=State) ->
    Args1 = merge_args(Args, RenderArgs),
    {IOList, Context1} = z_template:render_to_iolist(Template, Args1, Context),
    NewFrame = z_convert:to_binary(IOList),

    State1 = State#state{render_context=Context1},

    case z_teleview_differ:new_frame(DifferPid, NewFrame) of
        busy ->
            %% The differ could not handle the new frame. It will be dropped.
            lager:warning("Differ is busy. Could not update view", []),
            {ok, State1};
        ok ->
            {ok, State1}
    end.
 
% Get the pid of the differ from the supervisor.
get_differ_pid(SupervisorPid) when is_pid(SupervisorPid) ->
    get_differ_pid(supervisor:which_children(SupervisorPid));
get_differ_pid([]) ->
    undefined;
get_differ_pid([{z_teleview_differ, Pid, _, _}| _Rest]) ->
    Pid;
get_differ_pid([_Child | Rest]) ->
    get_differ_pid(Rest).

%% Merge arguments used for rendering. 
merge_args(Args, RenderArgs) when is_map(Args) andalso is_map(RenderArgs) ->
    maps:merge(Args, RenderArgs);
merge_args(Args, RenderArgs) when is_list(Args) andalso is_list(RenderArgs) ->
    z_utils:props_merge(Args, RenderArgs).
