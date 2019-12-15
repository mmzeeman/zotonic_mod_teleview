%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman
%% @doc TeleView renderer supervisor for a single render -> differ pipeline.

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

-module(z_teleview_renderer_sup).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

-export([publish_event/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MIN_TIME, 10000).
-define(MAX_TIME, 60000).

%%
%% Api
%%

start_link(Id, Args, Context) ->
    supervisor:start_link(
      {via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE,
      [Id, Args, Context]).

%%
%% supervisor callback
%%

init([Id, #{ render_ref := RenderRef } = Args, Context]) ->
    MinTime = maps:get(differ_min_time, Args, ?MIN_TIME),
    MaxTime = maps:get(differ_max_time, Args, ?MAX_TIME),

    EventTopic = <<"model/teleview/", Id/binary, $/, RenderRef/binary, "/event">>,

    {ok, {{one_for_all, 20, 10},
          [
           {z_teleview_render,
            {z_teleview_render, start_link, [RenderRef, Args, Context]},
            permanent, 5000, worker, dynamic},

           {z_teleview_differ,
            {z_teleview_differ, start_link, [RenderRef, MinTime, MaxTime,
                                             {?MODULE, publish_event, [EventTopic, Context]},
                                             Context]},
            permanent, 5000, worker, dynamic}
          ]}}.

%%
%% Helpers
%%

%% Publish the patch on a topic.
publish_event(Patch, [EventTopic, Context]) ->
    z_mqtt:publish(
      event_topic(EventTopic, Patch),
      patch_to_map(Patch),
      Context).

% Const
event_topic(Topic, {keyframe, _, _}) -> <<Topic/binary, "/keyframe">>;
event_topic(Topic, {cumulative, _, _}) -> <<Topic/binary, "/cdiff">>;
event_topic(Topic, {incremental, _, _}) -> <<Topic/binary, "/idiff">>.

% Convert the patch to a map which can be transported efficiently.
patch_to_map({keyframe, Data, Timestamp}) ->
    #{html => Data,ts => Timestamp};
patch_to_map({cumulative, Patch, Timestamp}) ->
    #{cdiff => transform_patch(Patch), ts => Timestamp};
patch_to_map({incremental, Patch, Timestamp}) ->
    #{idiff => transform_patch(Patch), ts => Timestamp}.

transform_patch(L) when is_list(L) ->
    transform_patch(L, []).

transform_patch([], Acc) ->
    lists:reverse(Acc);
transform_patch([{skip, I}|Rest], Acc) ->
    transform_patch(Rest, [I, <<$s>> | Acc]);
transform_patch([{copy, I}|Rest], Acc) ->
    transform_patch(Rest, [I, <<$c>> | Acc]);
transform_patch([{insert, Data}|Rest], Acc) ->
    transform_patch(Rest, [Data, <<$i>> | Acc]).


