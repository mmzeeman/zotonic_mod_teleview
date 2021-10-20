%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2021 Maas-Maarten Zeeman
%% @doc TeleView Differ.

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

-module(z_teleview_differ).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(gen_server).

-define(DEFAULT_MIN_TIME, 0).
-define(DEFAULT_MAX_TIME, infinite).

-define(PATCH_OVERHEAD, 15).

%% Generates diffs to update remote views with a minimum use of bandwith.

-record(state, {
          teleview_id,
          renderer_id,

          % The current keyframe
          keyframe :: undefined | binary(),
          keyframe_sn = 0 :: non_neg_integer(),
          last_time = 0 :: integer(),

          % The current frame
          current_frame :: undefined | binary(),
          current_frame_sn = 0 :: non_neg_integer(),

          new_frame :: undefined | binary(), % The frame which will be processed. 
          processing = false :: boolean(),

          min_time=?DEFAULT_MIN_TIME :: non_neg_integer(),        % minimum time between keyframes. 
          max_time=?DEFAULT_MAX_TIME :: pos_integer() | infinite, % integer in ms | infinite

          context :: zotonic:context()
}).

%% api
-export([
    start_link/4,
    new_frame/2,
    sync_new_frame/2,

    state/3
]).


-include_lib("zotonic_core/include/zotonic.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Api
%% 

start_link(TeleviewId, RendererId, Args, Context) -> 
    %% Use a new empty context for the differ to make the 
    %% published updates smaller.
    DifferContext = z_context:new(Context),
    gen_server:start_link({via, z_proc, {{?MODULE, TeleviewId, RendererId}, Context}},
                          ?MODULE,
                          [TeleviewId, RendererId, Args, DifferContext], []).
  
new_frame(Pid, NewFrame) ->
    gen_server:call(Pid, {new_frame, NewFrame}).

sync_new_frame(Pid, NewFrame) ->
    gen_server:call(Pid, {sync_new_frame, NewFrame}).

state(TeleviewId, RendererId, Context) ->
    gen_server:call({via, z_proc, {{?MODULE, TeleviewId, RendererId}, Context}},
                    state).


%%
%% gen_server callbacks
%%

init([TeleviewId, RendererId, Args, Context]) ->
    process_flag(trap_exit, true),

    % When we restarted because of an error, the viewers should be reset.
    m_teleview:publish_event(reset, TeleviewId, RendererId, #{}, Context),

    {ok, #state{min_time=maps:get(differ_min_time, Args, ?DEFAULT_MIN_TIME),
                max_time=maps:get(differ_max_time, Args, ?DEFAULT_MAX_TIME),
                teleview_id=TeleviewId,
                renderer_id=RendererId,
                context=Context}}.

%
handle_call({new_frame, _Frame}, _From, #state{processing=true}=State) ->
    % Notify the caller that the differ is processing.
    {reply, busy, State};
handle_call({new_frame, Frame}, _From, #state{processing=false}=State) ->
    case Frame =/= State#state.current_frame of
        true ->
            self() ! next_patch,
            {reply, ok, State#state{new_frame=Frame, processing=true}};
        false ->
            {reply, ok, State}
    end;

handle_call({sync_new_frame, Frame}, _From, #state{}=State) ->
    {_Patch, State1} = next_patch(Frame, State),
    {reply, differ_state(State1), State1#state{processing=false, new_frame=undefined}};


handle_call(state, _From, State) ->
    {reply, differ_state(State), State};

handle_call(keyframe, _From, State) ->
    {reply, State#state.keyframe, State};

handle_call(current_frame, _From, State) ->
    {reply, State#state.current_frame, State}.

%
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

%
handle_info(next_patch, #state{processing=true, new_frame=Frame}=State) ->
    {Patch, State1} = next_patch(Frame, State),
    broadcast_patch(Patch, State1),
    z_utils:flush_message(next_patch),
    {noreply, State1#state{processing=false, new_frame=undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    z_teleview_state:delete_frames(State#state.teleview_id, State#state.renderer_id, State#state.context),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% helpers
%%

broadcast_patch({keyframe, Msg}, State) ->
    m_teleview:publish_event(update, keyframe,
                             State#state.teleview_id, State#state.renderer_id,
                             Msg,
                             State#state.context);
broadcast_patch({incremental, Msg}, State) ->
    m_teleview:publish_event(update, incremental,
                             State#state.teleview_id, State#state.renderer_id,
                             Msg,
                             State#state.context);
broadcast_patch({cumulative, Msg}, State) ->
    m_teleview:publish_event(update, cumulative,
                             State#state.teleview_id, State#state.renderer_id,
                             Msg,
                             State#state.context).

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

    z_teleview_state:store_keyframe(State#state.teleview_id, State#state.renderer_id, NewFrame, State1#state.current_frame_sn, State#state.context),

    % And keep this frame as the new keyframe
    State1#state{keyframe = NewFrame,
                 keyframe_sn = State1#state.current_frame_sn,
                 last_time = CurrentTime
                }.

%% Update the state when a cumulative or incremental patch is produced.
update_state(NewFrame, State) ->
    FrameSn = State#state.current_frame_sn + 1,
    z_teleview_state:store_current_frame(State#state.teleview_id, State#state.renderer_id, NewFrame, FrameSn, State#state.context),

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


differ_state(#state{}=State) ->
    #{keyframe => State#state.keyframe,
      keyframe_sn => State#state.keyframe_sn,
      current_frame => State#state.current_frame,
      current_frame_sn => State#state.current_frame_sn,
      min_time => State#state.min_time,
      max_time => State#state.max_time,
      teleview_id => State#state.teleview_id,
      renderer_id => State#state.renderer_id
     }.

