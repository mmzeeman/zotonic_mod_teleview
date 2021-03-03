%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2021 Maas-Maarten Zeeman
%% @doc Teleview model.

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


-module(m_teleview).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(zotonic_model).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    m_get/3,
    m_post/3
]).

-export([
    publish_event/4,
    publish_event/5,
    publish_event/6
]).

%% @doc ...
%%
%% Interface update topics:
%%
%% model/teleview/get/<teleview-id>/state/<renderer-id>
%%
%% model/teleview/post/<teleview-id>/still-watching/<renderer-id>      : Indicate that the viewer is still watching.
%%
%%
%% model/teleview/event/<teleview-id>/stopped                          : The whole teleview is stopped. (e.g. when all renderers are gone) 
%% model/teleview/event/<teleview-id>/started                          : The teleview is started. 
%%
%% model/teleview/event/<teleview-id>/reset/<renderer-id>              : The viewer must be reset. Wait for new keyframe.
%% model/teleview/event/<teleview-id>/still-watching/<renderer-id>     : Reply to keep renderer alive
%% model/teleview/event/<teleview-id>/update/<renderer-id>/keyframe    : keyframe update.
%% model/teleview/event/<teleview-id>/update/<renderer-id>/cumulative  : a patch against the last keyframe.
%% model/teleview/event/<teleview-id>/update/<renderer-id>/incremental : a patch against the current frame.
%%

m_get(V, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, V]),
    {error, unknown_path}.

m_post([Teleview, <<"still_watching">>, Renderer | Rest], Msg, Context) ->
    z_teleview_state:keep_alive(z_convert:to_integer(Teleview), z_convert:to_integer(Renderer), Context),
    ok;
m_post(Topic, Msg, Context) ->
    ?DEBUG(Topic),

    ok.

%%
%% API
%%

publish_event(Event, TeleviewId, Msg, Context) ->
    z_mqtt:publish([model, teleview, event, TeleviewId, Event], Msg, z_acl:sudo(Context)).

publish_event(Event, TeleviewId, RendererId, Msg, Context) ->
    z_mqtt:publish([model, teleview, event, TeleviewId, Event, RendererId], Msg, z_acl:sudo(Context)).

publish_event(Event, SubEvent, TeleviewId, RendererId, Msg, Context) ->
    z_mqtt:publish([model, teleview, event, TeleviewId, Event, RendererId, SubEvent], Msg, z_acl:sudo(Context)).


