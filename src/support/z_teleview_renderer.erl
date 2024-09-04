%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2021 Maas-Maarten Zeeman
%% @doc TeleView Renderer.

%% Copyright 2021 Maas-Maarten Zeeman 
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
%% Responsible for rendering and diffing updated views... 
%%

-module(z_teleview_renderer).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-behaviour(gen_server).

-define(DEFAULT_MIN_TIME, 0).
-define(DEFAULT_MAX_TIME, infinite).

-define(PATCH_OVERHEAD, 15).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    start_link/4,

    is_already_started/3,

    render/3,
    render/4,

    sync_render/4
]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    teleview_id,
    renderer_id,

    args :: map(), % the arguments used to render
    template :: binary(), % the template to use to render

    % The current keyframe
    keyframe :: undefined | binary(),
    keyframe_sn = 0 :: non_neg_integer(),
    last_time = 0 :: integer(),

    % The current frame
    current_frame :: undefined | binary(),
    current_frame_sn = 0 :: non_neg_integer(),

    min_time=?DEFAULT_MIN_TIME :: non_neg_integer(),        % minimum time between keyframes. 
    max_time=?DEFAULT_MAX_TIME :: pos_integer() | infinite, % integer in ms | infinite

    context :: z:context() % the context to use to render with
}).


% Start a new renderer
start_link(TeleviewId, RendererId, Args, Context) ->
    RendererContext = renderer_context(Args, Context),
    gen_server:start_link({via, z_proc, {{?MODULE, TeleviewId, RendererId}, Context}},
                          ?MODULE,
                          [TeleviewId, RendererId, Args, RendererContext],
                          []).
 
%% Return true if the renderer is already started.
is_already_started(TeleviewId, RendererId, Context) ->
    is_pid(z_proc:whereis({?MODULE, TeleviewId, RendererId}, Context)).

% Render with pid.
render(Pid, Args, _Context) when is_pid(Pid) ->
    gen_server:cast(Pid, {render, Args}).

% Render teleview and renderer id. 
render(TeleviewId, RendererId, Args, Context) ->
    gen_server:cast({via, z_proc, {{?MODULE, TeleviewId, RendererId}, Context}}, {render, Args}).

% Do a render and wait until the render is finished.
sync_render(TeleviewId, RendererId, Args, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, TeleviewId, RendererId}, Context}}, {render, Args}).

%%
%% gen_server callbacks
%%

-spec init( list() ) -> {ok, term()}.
init([TeleviewId, RendererId, #{ template := Template }=Args, Context]) ->
    process_flag(trap_exit, true),

    % When we restarted because of an error, the viewers should be reset.
    m_teleview:publish_event(<<"reset">>, TeleviewId, RendererId, #{}, Context),

    Args1 = Args#{teleview_id => TeleviewId, renderer_id => RendererId},

    {ok, #state{
            teleview_id=TeleviewId,
            renderer_id=RendererId,
                
            template=Template,
            args=Args1,

            min_time=maps:get(keyframe_min_time, Args, ?DEFAULT_MIN_TIME),
            max_time=maps:get(keyframe_max_time, Args, ?DEFAULT_MAX_TIME),

            context=Context}}.


handle_call({render, Args}, _From, State) ->
    State1 = render_and_broadcast_patch(Args, State),
    {reply, ok, State1};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast({render, A}, State) ->
    %% Get the last render cast from the mailbox. Skip all others.
    Args = last_render_cast_args(A),
    State1 = render_and_broadcast_patch(Args, State),
    {noreply, State1};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    z_teleview_state:delete_frames(State#state.teleview_id, State#state.renderer_id, State#state.context),
    ok.

%%
%% Helpers
%%

render_and_broadcast_patch(Args, State) ->
    z_depcache:in_process(true),
    try
        RenderResult = render(Args, State),
        {Patch, State1} = next_patch(RenderResult, State),
        broadcast_patch(Patch, State1),
        State1
    after
        z_depcache:in_process(false)
    end.

%% 
renderer_context(Args, Context) ->
    case z_notifier:first({teleview_renderer_init, Args}, Context) of
        undefined ->
            z_acl:anondo(Context);
        #context{} = NewContext ->
            NewContext
    end.

%%
broadcast_patch({keyframe, Msg}, #state{teleview_id=TeleviewId, renderer_id=RendererId, context=Context}) ->
    z_mqtt:publish([<<"model">>, <<"teleview">>, <<"event">>, TeleviewId, <<"update">>, RendererId, <<"keyframe">>],
                   Msg, #{ qos => 1, retain => true },  z_acl:sudo(Context));

broadcast_patch({incremental, Msg}, #state{teleview_id=TeleviewId, renderer_id=RendererId, context=Context}) ->
    z_mqtt:publish([<<"model">>, <<"teleview">>, <<"event">>, TeleviewId, <<"update">>, RendererId, <<"incremental">>],
                   Msg, #{ qos => 1 }, z_acl:sudo(Context));

broadcast_patch({cumulative, Msg}, #state{teleview_id=TeleviewId, renderer_id=RendererId, context=Context}) ->
    z_mqtt:publish([<<"model">>, <<"teleview">>, <<"event">>, TeleviewId, <<"update">>, RendererId, <<"cumulative">>],
                   Msg, z_acl:sudo(Context)).

%% When there are more render casts in the mailbox, skip them, and
%% take arguments from the last message.
last_render_cast_args(Args) ->
    receive
        {'$gen_cast', {render, A}} ->
            last_render_cast_args(A)
    after
        0 -> Args
    end.
      
% Render the template with the supplied vars and send the result to the differ.
render(Args, #state{template=Template, args=RenderArgs, context=Context}) ->
    Args1 = merge_args(RenderArgs, Args),
    {IOList, _Context} = z_template:render_to_iolist(Template, Args1, Context),
    z_convert:to_binary(IOList).

%% Merge arguments used for rendering. 
merge_args(Args, RenderArgs) ->
    maps:merge(Args, RenderArgs).


%% Create the next patch
%%
next_patch(NewFrame, #state{keyframe=undefined}=State) ->
    State1 = update_state_keyframe(NewFrame, current_time(), State),
    {{keyframe, #{ frame => NewFrame, keyframe_sn => State1#state.keyframe_sn}}, State1};
next_patch(NewFrame, #state{max_time=infinite}=State) ->
    do_patch(NewFrame, State);
next_patch(NewFrame, #state{max_time=MaxTime}=State) ->
    CurrentTime = current_time(),
    case (CurrentTime - State#state.last_time) > MaxTime of
        true ->
            State1 = update_state_keyframe(NewFrame, CurrentTime, State),
            {{keyframe, #{ frame => NewFrame, keyframe_sn => State1#state.keyframe_sn }}, State1 };
        false ->
            do_patch(NewFrame, State)
    end.

%% Make a patch against the current keyframe, and check for complexity.
%%
do_patch(NewFrame, #state{keyframe=KeyFrame}=State) ->
    Patch = make_patch(KeyFrame, NewFrame),

    case is_complexity_too_high(Patch, NewFrame) of
        true ->
            %% Now we either create a new keyframe update,
            %% or an incremental patch. Depending on the situation
            do_complex_patch(NewFrame, State);
        false ->
            do_cumulative_patch(NewFrame, Patch, State)
    end.

%% Create a cumulative patch
%%
do_cumulative_patch(NewFrame, Patch, #state{min_time=0}=State) ->
    %% When min_time == 0 there are no incremental frames, so the current_frame_sn is not added to the patch.
    State1 = update_state(NewFrame, State),
    {{cumulative, #{patch => patch_to_list(Patch),
                    keyframe_sn => State#state.keyframe_sn,
                    result_size => size(NewFrame)
                   }},
     State1};
do_cumulative_patch(NewFrame, Patch, #state{}=State) ->
    %% In this case it is possible that the viewers get incremental updates. The current_frame_sn
    %% must be included too.
    State1 = update_state(NewFrame, State),
    {{cumulative, #{ patch => patch_to_list(Patch),
                     keyframe_sn => State1#state.keyframe_sn,
                     current_frame_sn => State1#state.current_frame_sn,
                     result_size => size(NewFrame)
                   }},
     State1}.

%% Create either a keyframe, or an incremental patch, depending
%% on the passed minimum time.
%%
do_complex_patch(NewFrame, #state{min_time=0}=State) ->
    %% Keep it simple, just make a new keyframe
    State1 = update_state_keyframe(NewFrame, current_time(), State),
    {{keyframe, #{frame => NewFrame, keyframe_sn => State1#state.keyframe_sn}}, State1};
do_complex_patch(NewFrame, #state{}=State) ->
    CurrentTime = current_time(),

    case (CurrentTime - State#state.last_time) > State#state.min_time of
        true ->
            %% The last keyframe was too long ago, just send one.
            State1 = update_state_keyframe(NewFrame, CurrentTime, State),
            {{keyframe, #{frame => NewFrame, keyframe_sn => State1#state.keyframe_sn}}, State1};
        false ->
            %% Make a patch against the current_frame. (Assumes this patch is smaller)
            Patch = make_patch(State#state.current_frame, NewFrame),
            State1 = update_state(NewFrame, State),
            {{incremental, #{ patch => patch_to_list(Patch),
                              keyframe_sn => State1#state.keyframe_sn,
                              current_frame_sn => State1#state.current_frame_sn,
                              result_size => size(NewFrame)
                            }},
             State1}
    end.


%% Update the state when a keyframe is produced.
update_state_keyframe(NewFrame, CurrentTime, State) ->
    % Update the state normally
    State1 = update_state(NewFrame, State),

    z_teleview_state:store_keyframe(State#state.teleview_id,
                                    State#state.renderer_id,
                                    NewFrame,
                                    State1#state.current_frame_sn,
                                    State#state.context),

    % And keep this frame as the new keyframe
    State1#state{keyframe = NewFrame,
                 keyframe_sn = State1#state.current_frame_sn,
                 last_time = CurrentTime
                }.

%% Update the state when a cumulative or incremental patch is produced.
update_state(NewFrame, State) ->
    FrameSn = State#state.current_frame_sn + 1,
    z_teleview_state:store_current_frame(State#state.teleview_id,
                                         State#state.renderer_id,
                                         NewFrame, FrameSn,
                                         State#state.context),

    State#state{
      current_frame = NewFrame,
      current_frame_sn = FrameSn
     }.

make_patch(SourceText, DestinationText) ->
    Diffs = diffy:diff(SourceText, DestinationText),
    CleanedDiffs = diffy:cleanup_efficiency(Diffs),
    diffy_simple_patch:make_patch(CleanedDiffs).

is_complexity_too_high(Diffs, Doc) ->
    Size = size(Doc),
    EstimatedSize = estimate_size(Diffs),
    ?PATCH_OVERHEAD + (EstimatedSize * 1.5)  > Size.

estimate_size(Diffs) ->
    estimate_size(Diffs, 0).

estimate_size([], Acc) -> Acc;
estimate_size([{insert, Data}|Rest], Acc) ->
    estimate_size(Rest, Acc + 4 + estimate_size_element(Data));
estimate_size([{copy, N}|Rest], Acc) ->
    estimate_size(Rest, Acc + 4 + estimate_size_element(N));
estimate_size([{skip, N}|Rest], Acc) ->
    estimate_size(Rest, Acc + 4 + estimate_size_element(N)).

estimate_size_element(I) when is_integer(I) andalso I < 10 -> 1;
estimate_size_element(I) when is_integer(I) andalso I < 100 -> 2;
estimate_size_element(I) when is_integer(I) andalso I < 1000 -> 3;
estimate_size_element(I) when is_integer(I) andalso I < 10000 -> 4;
estimate_size_element(I) when is_integer(I) andalso I < 100000 -> 5;
estimate_size_element(I) when is_integer(I) -> z_convert:to_integer(math:log10(I));
estimate_size_element(B) when is_binary(B) -> size(B).

current_time() ->
    erlang:system_time(millisecond).

patch_to_list(Patch) ->
    patch_to_list(Patch, []).
    
patch_to_list([], Acc) ->
    lists:reverse(Acc);
patch_to_list([{copy, N} | Rest], Acc) ->
    patch_to_list(Rest, [N, c | Acc]);
patch_to_list([{skip, N} | Rest], Acc) ->
    patch_to_list(Rest, [N, s | Acc]);
patch_to_list([{insert, Bin} | Rest], Acc) ->
    patch_to_list(Rest, [Bin, i | Acc]).


