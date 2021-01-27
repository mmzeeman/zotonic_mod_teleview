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
    start_link/4,
    render/3,
    render/4
]).


% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {
          render_ref,

          template,
          differ_pid,

          context
}).

%%
%% api
%%

start_link(TeleviewId, SupervisorPid, #{ render_ref := RenderRef} = Args, Context) ->
    gen_server:start_link({via, z_proc, {{?MODULE, TeleviewId, RenderRef}, Context}},
                          ?MODULE,
                          [SupervisorPid, Args, Context], []).


% render without arguments
render(TeleviewId, RenderRef, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, TeleviewId, RenderRef}, Context}}, render).

% render with arguments
render(TeleviewId, RenderRef, Args, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, TeleviewId, RenderRef}, Context}}, {render, Args}).

%%
%% gen_server callbacks.
%%

init([Supervisor, #{render_ref := RenderRef, template := Template}=Args, Context]) ->
    ?DEBUG(render_start),

    self() ! {get_differ_pid, Supervisor},

    {ok, #state{render_ref=RenderRef, template=Template, context=Context}}.

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

%handle_info({start_differ, Supervisor, #{differ_event_mfa:={_,_,_}=EventMFA}=Args}, State) ->
%    %% Start the differ as a separate process so it functions as a pipeline.
%    MinTime = maps:get(differ_min_time, Args, ?MIN_TIME),
%    MaxTime = maps:get(differ_max_time, Args, ?MAX_TIME),
%
%    MFA = {z_teleview_differ, start_link, [MinTime, MaxTime, EventMFA]},
%
%    DifferSpec = #{id => z_teleview_differ,
%                   start => MFA,
%                   shutdown => 1000,
%                   type => worker,
%                   modules => [z_teleview_differ]},
%
%    {ok, Pid} = supervisor:start_child(Supervisor, DifferSpec),
%    link(Pid),
%    {noreply, State#state{differ_pid=Pid}}; 
handle_info({get_differ_pid, Supervisor}, State) ->
    ?DEBUG({sup_pid, Supervisor}),

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
render_and_diff(Vars, #state{template=Template, context=Context, differ_pid=DifferPid}=State) ->
    {IOList, Context1} = z_template:render_to_iolist(Template, Vars, Context),
    NewFrame = z_convert:to_binary(IOList),

    State1 = State#state{context=Context1},

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
    get_differ_pid(?DEBUG(supervisor:which_children(SupervisorPid)));
get_differ_pid([]) ->
    undefined;
get_differ_pid([{z_teleview_differ, Pid, _, _}| _Rest]) ->
    Pid;
get_differ_pid([_Child | Rest]) ->
    get_differ_pid(Rest).

