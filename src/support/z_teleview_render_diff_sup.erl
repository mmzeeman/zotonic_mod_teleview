%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman
%% @doc TeleView Render Diff Supervisor.

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

-module(z_teleview_render_diff_sup).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

-export([publish_patch/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MIN_TIME, 10000).
-define(MAX_TIME, 60000).

start_link(Id, Args, Context) ->
    supervisor:start_link(
      {via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE,
      [Id, Args, Context]).

init([Id, Args, Context]) ->
    MinTime = find_config(differ_min_time, Args, ?MIN_TIME),
    MaxTime = find_config(differ_max_time, Args, ?MAX_TIME),

    {ok, {{one_for_all, 20, 10},
          [
           {z_teleview_render,
            {z_teleview_render, start_link, [Id, Args, Context]},
            permanent, 5000, worker, dynamic},

           {z_teleview_differ,
            {z_teleview_differ, start_link, [Id, MinTime, MaxTime, {?MODULE, publish_patch, Context}, Context]},
            permanent, 5000, worker, dynamic}
          ]}}.

%% Publish the patch on a topic.
publish_patch(Patch, _Context) ->
    %% Convert the patch to JSON
    io:fwrite(standard_error, <<"~s~n">>, [patch_to_json(Patch)]).

patch_to_json({key_frame, Data, Timestamp}) ->
    z_json:encode([{html, Data}, {ts, Timestamp}]);

patch_to_json({cumulative, Patch, Timestamp}) ->
    z_json:encode([{cdiff, transform_patch(Patch, [])}, {ts, Timestamp}]);
patch_to_json({incremental, Patch, Timestamp}) ->
    z_json:encode([{idiff, transform_patch(Patch, [])}, {ts, Timestamp}]).


transform_patch([], Acc) ->
    lists:reverse(Acc);
transform_patch([{skip, I}|Rest], Acc) ->
    transform_patch(Rest, [I, <<$s>> | Acc]);
transform_patch([{copy, I}|Rest], Acc) ->
    transform_patch(Rest, [I, <<$c>> | Acc]);
transform_patch([{insert, Data}|Rest], Acc) ->
    transform_patch(Rest, [Data, <<$i>> | Acc]).




%% 
%% Helpers
%%

find_config(Key, Map, Default) ->
    case maps:find(Key, Map) of
        error -> Default;
        Value -> Value
    end.

