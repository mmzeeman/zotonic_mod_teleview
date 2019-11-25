%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman
%% @doc TeleView Differ.

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

-module(z_teleview_differ).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(gen_server).

%% Generates diffs to update remote views with a minimum use of bandwith.

-record(state, {
          id,

          key_frame,
          current_frame,
          last_time=0,

          new_frame,

          min_time=10000, 
          max_time=60000,  % integer in ms | infinite

          mfa=undefined,

          processing=false,
          context :: zotonic:context()
}).

%% api
-export([
    start_link/5
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Need to pass, broadcast function, min and maxtime, 
%% 

start_link(Id, MinTime, MaxTime, {_M, _F, _A}=MFA, Context) ->
    gen_server:start_link({via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE, [Id, MinTime, MaxTime, MFA, Context], []).

%% gen_server callbacks

init([Id, MinTime, MaxTime, MFA, Context]) ->
    {ok, #state{id=Id, min_time=MinTime, max_time=MaxTime, mfa=MFA, context=Context}}.

%
handle_call({new_frame, _Frame}, _From, #state{processing=true}=State) ->
    % The update will be dropped.
    {reply, busy, State};
handle_call({new_frame, Frame}, _From, #state{processing=false}=State) ->
    self() ! next_patch,
    {reply, ok, State#state{new_frame=Frame, processing=true}};

handle_call(key_frame, _From, State) ->
    {reply, State#state.key_frame, State};

handle_call(current_frame, _From, State) ->
    {reply, State#state.current_frame, State}.

%
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

%
handle_info(next_patch, #state{processing=true,
                               new_frame=Frame,
                               key_frame=Key,
                               current_frame=Current,
                               last_time=LastTime}=State) ->
    Patch = next_patch(Frame, Current, Key, current_time(), LastTime, State#state.min_time, State#state.max_time),
    broadcast_patch(Patch, State),
    case Patch of
        {key_frame, _, CurrentTime} ->
            {noreply, State#state{
                        key_frame=Frame,
                        current_frame=Frame,
                        last_time=CurrentTime,
                        processing=false}};
        _ ->
            {noreply, State#state{
                        current_frame=Frame,
                        processing=false}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% helpers
%%

broadcast_patch(Patch, #state{mfa={Module, Function, Args}}) ->
    Module:Function(Patch, Args).

%% Calculate the next patch.
next_patch(Frame, Current, Key, CurrentTime, LastTime, MinTime, infinite) ->
    DeltaTime = CurrentTime - LastTime,
    next_patch1(Frame, Current, Key, CurrentTime, DeltaTime, MinTime);
next_patch(Frame, Current, Key, CurrentTime, LastTime, MinTime, MaxTime) ->
    DeltaTime = CurrentTime - LastTime,

    case DeltaTime > MaxTime of
        true ->
            {key_frame, Frame, CurrentTime};
        false ->
            next_patch1(Frame, Current, Key, CurrentTime, DeltaTime, MinTime)
    end.

next_patch1(Frame, Current, Key, CurrentTime, DeltaTime, MinTime) ->
    CumulativePatch = make_patch(Key, Frame),
    case complexity(CumulativePatch, Frame) of
        too_high ->
            case DeltaTime > MinTime of
                true ->
                    {key_frame, Frame, CurrentTime};
                false ->
                    IncrementalPatch = make_patch(Current, Frame),
                    {incremental, IncrementalPatch, CurrentTime}
            end;
        _ ->
            {cumulative, CumulativePatch, CurrentTime}
    end.


make_patch(SourceText, DestinationText) ->
    Diffs = diffy:diff(SourceText, DestinationText),
    CleanedDiffs = diffy:cleanup_efficiency(Diffs),
    diffy_simple_patch:make_patch(CleanedDiffs).

complexity(Diffs, Doc) ->
    Size = size(Doc),
    EstimatedSize = estimate_size(Diffs),
    case EstimatedSize > Size of
        true -> too_high;
        false -> ok
    end.

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


%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

estimate_size_test() ->
    ?assertEqual(estimate_size([]), 0),
    ?assertEqual(8, estimate_size([{skip, 1000}])),
    ?assertEqual(5, estimate_size([{skip, 1}])),
    ?assertEqual(8, estimate_size([{insert, <<"toot">>}])),

    ok.


next_frame_test() ->
    P1 = next_patch(<<"jungle">>, <<"jungle">>, <<"jungle">>, 100, 0, 10, 2000),
    ?assertEqual({cumulative, [], 100}, P1),

    P2 = next_patch(<<"jungle">>, <<"jungle">>, <<"jungle">>, 2001, 0, 10, 2000),
    ?assertEqual({key_frame, <<"jungle">>, 2001}, P2),

    %% Infinite maxtime
    P3 = next_patch(<<"jungle">>, <<"jungle">>, <<"jungle">>, 2001, 0, 10, infinite),
    ?assertEqual({cumulative, [], 2001}, P3),

    ok.

make_patch_test() ->
    P1 = make_patch(<<"jungle">>, <<"jungle">>),
    ?assertEqual([], P1),

    P2 = make_patch(<<"aab">>, <<"aabb">>),
    ?assertEqual([{copy,4},{skip,1}], P2),

    ok.


-endif.
