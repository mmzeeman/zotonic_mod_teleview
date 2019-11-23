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

-define(MIN_TIME, 10000).
-define(MAX_TIME, 60000).

-record(state, {
          id,

          key_frame,
          current_frame,
          last_time=0,

          new_frame,

          processing=false,
          context :: zotonic:context()
}).

%% api
-export([
    start_link/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Id, Context) ->
    gen_server:start_link({via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE, [Id, Context], []).

%% gen_server callbacks

init([Id, Context]) ->
    {ok, #state{id=Id, context=Context}}.

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
    Patch = next_patch(Frame, Current, Key, LastTime, ?MIN_TIME, ?MAX_TIME),

    case Patch of
        {keydoc, _, CurrentTime} ->
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

%% Calculate the next patch.
next_patch(Frame, Current, Key, LastTime, MinTime, MaxTime) ->
    CurrentTime = current_time(),
    DeltaTime = CurrentTime - LastTime,

    case DeltaTime > MaxTime of
        true ->
            {keydoc, Frame, CurrentTime};
        false ->
            CumulativePatch = make_patch(Key, Frame),
            case complexity(CumulativePatch, Frame) of
                too_high ->
                    case DeltaTime > MinTime of
                        true ->
                            {keydoc, Frame, CurrentTime};
                        false ->
                            IncrementalPatch = make_patch(Current, Frame),
                            {incremental, IncrementalPatch, CurrentTime}
                    end;
                _ ->
                    {cumulative, CumulativePatch, CurrentTime}
            end
    end.

make_patch(SourceText, DestinationText) ->
    Diffs = diffy:diff(SourceText, DestinationText),
    CleanedDiffs = diffy:cleanup_efficiency(Diffs),
    diffy_simple_patch:make_patch(CleanedDiffs).

complexity(Diffs, Doc) ->
    Size = size(Doc),
    EstimatedSize = estimated_size(Diffs),
    case EstimatedSize * 10 > Size of
        true -> too_high;
        false -> ok
    end.

estimated_size(Diffs) ->
    estimated_size(Diffs, length(Diffs) * 3).

estimated_size([], Acc) ->
    25 + Acc;
estimated_size([{i, Data}|Rest], Acc) ->
    estimated_size(Rest, Acc + 4 + size(Data));
estimated_size([{c, _N}|Rest], Acc) ->
    estimated_size(Rest, Acc + 6);
estimated_size([{s, _N}|Rest], Acc) ->
    estimated_size(Rest, Acc + 6).

current_time() ->
    erlang:system_time(millisecond).

