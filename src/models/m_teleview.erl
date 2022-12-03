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

%%
%% Interface update topics:
%%
%% model/teleview/get/<teleview-id>/state/<renderer-id> : Get the state of the renderer, ensures that it is running.
%%
%% model/teleview/post/<teleview-id>/still-watching/<renderer-id>      : Indicate that the viewer is still watching.
%%
%% model/teleview/event/<teleview-id>/stopped                          : The whole teleview is stopped. (e.g. when all renderers are gone) 
%% model/teleview/event/<teleview-id>/started                          : The teleview is started. 
%%
%% model/teleview/event/<teleview-id>/reset/<renderer-id>              : The viewer must be reset. Wait for new keyframe.
%% model/teleview/event/<teleview-id>/still-watching/<renderer-id>     : Reply to keep renderer alive
%% model/teleview/event/<teleview-id>/update/<renderer-id>/keyframe    : A keyframe update, update the entire view.
%% model/teleview/event/<teleview-id>/update/<renderer-id>/cumulative  : Patch against the last keyframe and update view.
%% model/teleview/event/<teleview-id>/update/<renderer-id>/incremental : Patch against the current frame and update view.
%%

%%
%% Model Gets
%%

%% Request for the keyframe
m_get([Teleview, <<"keyframe">>, Renderer | Rest], _Msg, Context) ->
    TeleviewId = z_convert:to_integer(Teleview),
    RendererId = z_convert:to_integer(Renderer),

    case z_teleview_acl:is_view_allowed(TeleviewId, RendererId, Context) of
        true ->
            Frame = z_teleview_state:get_keyframe(TeleviewId, RendererId, Context),
            {ok, {Frame, Rest}};
        false ->
            {error, eaccess}
    end;

%% Request for the current frame
m_get([Teleview, <<"current_frame">>, Renderer | Rest], _Msg, Context) ->
    TeleviewId = z_convert:to_integer(Teleview),
    RendererId = z_convert:to_integer(Renderer),

    case z_teleview_acl:is_view_allowed(TeleviewId, RendererId, Context) of
        true ->
            case z_teleview_state:get_current_frame(TeleviewId, RendererId, Context) of
                #{} = Frame -> 
                    {ok, {Frame, Rest}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eaccess}
    end;

m_get(V, _Msg, _Context) ->
    ?LOG_INFO("Unknown ~p lookup: ~p", [?MODULE, V]),
    {error, unknown_path}.

%%
%% Model Posts
%%

m_post([Teleview, <<"still_watching">>, Renderer], _Msg, Context) ->
    TeleviewId = z_convert:to_integer(Teleview),
    RendererId = z_convert:to_integer(Renderer),

    case z_teleview_acl:is_view_allowed(TeleviewId, RendererId, Context) of
        true ->
            z_teleview_state:keep_alive(TeleviewId, RendererId, Context);
        false ->
            {error, eaccess}
    end;
m_post(V, _Msg, _Context) ->
    ?LOG_INFO("Unknown ~p post: ~p", [?MODULE, V]),
    {error, unknown_path}.

%%
%% Model Events
%%

publish_event(Event, TeleviewId, Msg, Context) ->
    z_mqtt:publish([model, teleview, event, TeleviewId, Event], Msg, z_acl:sudo(Context)).

publish_event(Event, TeleviewId, RendererId, Msg, Context) ->
    z_mqtt:publish([model, teleview, event, TeleviewId, Event, RendererId], Msg, z_acl:sudo(Context)).

publish_event(Event, SubEvent, TeleviewId, RendererId, Msg, Context) ->
    z_mqtt:publish([model, teleview, event, TeleviewId, Event, RendererId, SubEvent], Msg, z_acl:sudo(Context)).


