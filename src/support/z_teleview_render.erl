%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2021 Maas-Maarten Zeeman
%% @doc TeleView Render.

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

-define(MAX_DIFF_TRIES, 3).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {
    template :: binary(), % the template to use to render
    args :: map(), % the arguments used to render
    render_context :: zotonic:context(), % the context to use to render with

    processing = false :: boolean(), % To indicate that the process is busy processing 
    render_args :: undefined | map(), % The arguments passed in the message triggering a render
    render_result :: undefined | binary(), % Stored render result for when the differ is busy. 
    diff_tries = 0 :: non_neg_integer(), % Number of times a render result is passed to the differ without success.

    differ_pid = undefined :: undefined | pid() % The pid of the differ.
}).

%%
%% api
%%

start_link(TeleviewId, RendererId, SupervisorPid, Args, Context) ->
    gen_server:start_link({via, z_proc, {{?MODULE, TeleviewId, RendererId}, Context}},
                          ?MODULE,
                          [SupervisorPid, TeleviewId, RendererId, Args, Context], []).

% render with pid.
render(Pid, Args, _Context) when is_pid(Pid) ->
    gen_server:call(Pid, {render, Args}).

% render teleview and renderer id. 
render(TeleviewId, RendererId, Args, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, TeleviewId, RendererId}, Context}}, {render, Args}).

%%
%% gen_server callbacks.
%%

init([Supervisor, TeleviewId, RendererId, #{<<"template">> := Template}=Args, Context]) ->
    %% init, otherwise the supervisor will deadlock causing a timeout.
    self() ! {get_differ_pid, Supervisor},

    %% Add the teleview_id and the renderer_id to the arguments.
    Args1 = Args#{teleview_id => TeleviewId,
                  renderer_id => RendererId},

    {ok, #state{template=Template,
                args=Args1,
                render_context=Context}}.

handle_call({render, Args}, _From, #state{processing = true}=State) ->
    %% We got even newer state... update the args, but wait for the timer.
    %%
    %% Note, the diff_tries is not updated. It could be that the differ is too
    %% slow, and can't keep up with the pace of the updates.
    lager:warning("Renderer is waiting for differ, but accepting new render.", []),
    {reply, ok, State#state{
                  render_result = undefined,
                  render_args = Args}};
handle_call({render, Args}, _From, #state{processing = false}=State) ->
    self() ! render,
    {reply, ok, State#state{
                  render_result = undefined,
                  render_args = Args,
                  processing = true}};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

%% Do a render.

% Tried to send the result to the differ too many times.
handle_info(render, #state{diff_tries=N}=State) when N > ?MAX_DIFF_TRIES ->
    lager:warning("Differ is too busy, dropping render result.", []),
    {noreply, State#state{diff_tries=0, render_result=undefined, render_args=undefined, processing=false}};
% There is no render result yet.
handle_info(render, #state{render_result=undefined, render_args=Args, processing=true}=State) when is_map(Args) ->
    z_utils:flush_message(render),
    RenderResult = render(Args, State),
    handle_render_result(RenderResult, State);
% There is a render result
handle_info(render, #state{render_result=RenderResult, render_args=undefined, processing=true}=State) when is_binary(RenderResult) ->
    z_utils:flush_message(render),
    handle_render_result(RenderResult, State);
    
%% Get the differ pid
handle_info({get_differ_pid, Supervisor}, State) ->
    case get_differ_pid(Supervisor) of
        undefined ->
            {noreply, stop, {error, no_differ}}; 
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

handle_render_result(RenderResult, State) ->
    case z_teleview_differ:new_frame(State#state.differ_pid, RenderResult) of
        ok ->
            {noreply, State#state{diff_tries = 0, render_result = undefined, render_args=undefined, processing=false}};
        busy ->
            RetryTime = diff_wait_time(State#state.diff_tries),
            lager:warning("Differ is busy, retry after ~p", [RetryTime]),
            erlang:send_after(RetryTime, self(), render),
            {noreply, State#state{diff_tries = State#state.diff_tries + 1,
                                  render_result=RenderResult,
                                  render_args=undefined,
                                  processing=true}}
    end.

diff_wait_time(N) ->
    MinWait = z_convert:to_integer(math:pow(10, N)),
    MaxWait = MinWait * 4,
    MinWait + z_ids:number(MaxWait).

% Render the template with the supplied vars and send the result to the differ.
render(Args, #state{template=Template, args=RenderArgs, render_context=Context}) ->
    Args1 = merge_args(Args, RenderArgs),
    {IOList, _Context} = z_template:render_to_iolist(Template, Args1, Context),
    z_convert:to_binary(IOList).

 
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

