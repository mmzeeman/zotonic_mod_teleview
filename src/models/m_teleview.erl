%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2026 Maas-Maarten Zeeman
%% @doc Teleview model.

%% Copyright 2019-2026 Maas-Maarten Zeeman
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
    publish_event/5
]).

%%
%% Interface update topics:
%%
%% model/teleview/get/<teleview-id>/state/<renderer-id> : Get the state of the renderer, ensures that it is running.
%%
%% model/teleview/post/<teleview-id>/ping/<renderer-id> : Indicate that a viewer is still watching.
%%
%% model/teleview/event/<teleview-id>/start/<renderer-id> : The viewer must be reset. Wait for new keyframe.
%% model/teleview/event/<teleview-id>/down/<renderer-id>  : The server side renderer is down, the view may restart it. 
%% model/teleview/event/<teleview-id>/ping/<renderer-id>  : Reply to keep renderer alive
%% model/teleview/event/<teleview-id>/ke/<renderer-id>    : A keyframe update, update the entire view.
%% model/teleview/event/<teleview-id>/cu/<renderer-id>    : Cumulative patch against the last keyframe and update view.
%% model/teleview/event/<teleview-id>/in/<renderer-id>    : Incremental patch against the current frame and update view.
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
            {error, eacces}
    end;

%% Request for the current frame
m_get([Teleview, <<"current_frame">>, Renderer | Rest], #{ payload := Payload }, Context) when is_map(Payload) ->
    TeleviewId = z_convert:to_integer(Teleview),
    RendererId = z_convert:to_integer(Renderer),

    case z_teleview_acl:is_view_allowed(TeleviewId, RendererId, Context) of
        true ->
            case z_teleview_state:get_current_frame(TeleviewId, RendererId, Payload, Context) of
                #{} = Frame -> 
                    {ok, {Frame, Rest}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end;

m_get([Teleview, <<"current_frame">>, Renderer | Rest], #{ }, Context) ->
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
            {error, eacces}
    end;

m_get([Teleview, <<"tick">> | Rest], #{ }, Context) ->
    TeleviewId = z_convert:to_integer(Teleview),
    case z_teleview_acl:is_post_allowed(TeleviewId, Context) of
        true ->
            case z_teleview_state:get_tick(TeleviewId, Context) of
                {ok, Tick} ->
                    {ok, {Tick, Rest}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end;

m_get(V, _Msg, _Context) ->
    ?LOG_INFO("Unknown ~p lookup: ~p", [?MODULE, V]),
    {error, unknown_path}.

%%
%% Model Posts
%%

m_post([Teleview, <<"ping">>, Renderer], _Msg, Context) ->
    TeleviewId = z_convert:to_integer(Teleview),
    RendererId = z_convert:to_integer(Renderer),
    case z_teleview_acl:is_view_allowed(TeleviewId, RendererId, Context) of
        true ->
            z_teleview_renderer:keep_alive(TeleviewId, RendererId, Context);
        false ->
            {error, eacces}
    end;
m_post([Teleview, <<"tick">> | Path], #{ payload := Payload }, Context) ->
    TeleviewId = z_convert:to_integer(Teleview),
    case z_teleview_acl:is_post_allowed(TeleviewId, Context) of
        true ->
            TickValue = get_tick_value(Path, Payload, Context),
            z_teleview_state:update_tick(TeleviewId, TickValue, Context);
        false ->
            {error, eacces}
    end;
%m_post([Teleview, <<"topics">> | Path], #{ payload := Payload }, Context) ->
%    TeleviewId = z_convert:to_integer(Teleview),
%    case z_teleview_acl:is_post_allowed(TeleviewId, Context) of
%        true ->
%            Topics = get_topics(Path, Payload, Context),
%            z_teleview_state:update_topics(TeleviewId, Topics, Context);
%        false ->
%            {error, eacces}
%    end;
m_post([Teleview, <<"state">> | Path], Msg, Context) ->
    TeleviewId = z_convert:to_integer(Teleview),
    case z_teleview_acl:is_post_allowed(TeleviewId, Context) of
        true ->
            z_teleview_state:post(TeleviewId, Path, Msg, Context);
        false ->
            {error, eacces}
    end;
m_post(V, _Msg, _Context) ->
    ?LOG_INFO("Unknown ~p post: ~p", [?MODULE, V]),
    {error, unknown_path}.

%%
%% Model Events
%%

-spec publish_event(binary(), integer(), integer(), term(), z:context()) -> ok | {error, term()}.
publish_event(Event, TeleviewId, RendererId, Msg, Context) ->
    z_mqtt:publish([<<"model">>, <<"teleview">>, <<"event">>, TeleviewId, Event, RendererId], Msg, z_acl:sudo(Context)).

-spec publish_event(binary(), integer(), term(), z:context()) -> ok | {error, term()}.
publish_event(Event, TeleviewId, Msg, Context) ->
    z_mqtt:publish([<<"model">>, <<"teleview">>, <<"event">>, TeleviewId, Event], Msg, z_acl:sudo(Context)).

%%
%% Helpers
%%

get_tick_value(Path, Payload, Context) ->
    Value = case Path of 
                [V] ->
                    %% The path is the value.
                    V;
                _ when is_map(Payload) ->
                    %% Take the value from the payload, either as a payload value, a data attribute or a context value
                    get_q(<<"tick">>, Payload, Context);
                _ ->
                    %% The payload is the value itself
                    Payload
            end,
    to_undefined_or_integer(Value).

get_topics(Path, Payload, Context) ->
    ?DEBUG(Path),
    ?DEBUG(Payload),
    [].

get_q(Name, Payload, Context) ->
    case maps:get(Name, Payload, undefined) of
        undefined ->
            case maps:get(<<"data-", Name/binary>>, maps:get(<<"message">>, Payload, #{}), undefined) of
                undefined ->
                    z_context:get_q(Name, Context);
                Value ->
                    Value
            end;
        Value ->
            Value
    end.

to_undefined_or_integer(<<"undefined">>) -> undefined;
to_undefined_or_integer(Value) -> z_convert:to_integer(Value).

