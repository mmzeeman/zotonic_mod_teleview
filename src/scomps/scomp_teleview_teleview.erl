%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2021 Maas-Maarten Zeeman
%% @doc Put a teleview on the page.

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


-module(scomp_teleview_teleview).
-behaviour(zotonic_scomp).

-export([vary/2, render/3, ensure_renderer/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

%% Make a place for the view to land and setup the worker which
%% is going to manage the view.

render(Params, _Vars, Context) ->
    Topics = proplists:get_all_values(topic, Params),
    RendererArgs = proplists:get_value(vary, Params, #{}),

    Args = maps:from_list(z_utils:prop_delete(topic, z_utils:prop_delete(vary, Params))),
    TeleviewArgs = maps:put(topics, Topics, Args),

    case ensure_renderer(TeleviewArgs, RendererArgs, Context) of
        {ok, TeleviewId, RendererId} ->
            %% Store the arguments of the teleview, they can be used later to restart
            %% the teleview.
            z_teleview_acl:store_args([{{teleview, TeleviewId}, TeleviewArgs},
                                       {{renderer, TeleviewId, RendererId}, RendererArgs}], Context),

            render_teleview(#{ teleview_id => TeleviewId,
                               renderer_id => RendererId,
                               teleview_wrapper_element => teleview_wrapper_element(TeleviewArgs),
                               teleview_wrapper_class => teleview_wrapper_class(TeleviewArgs),
                               keyframe_min_time => keyframe_min_time(TeleviewArgs),
                               keyframe_max_time => keyframe_max_time(TeleviewArgs) },
                            Params, Context);
        {error, _E}=Error ->
            Error
    end.

%%
%% Helpers
%%

ensure_renderer(undefined, undefined, _Context) ->
    {error, no_args};
ensure_renderer(TeleviewArgs, RendererArgs, Context) ->
    case mod_teleview:start_teleview(TeleviewArgs, Context) of
        {ok, TeleviewId} ->
            {ok, RendererId} = mod_teleview:start_renderer(TeleviewId, RendererArgs, Context),
            {ok, TeleviewId, RendererId};
        {error, _E}=Error ->
            Error
    end.

render_teleview(#{ teleview_id := TeleviewId,
                   renderer_id := RendererId,
                   teleview_wrapper_element := TeleviewWrapperElement,
                   teleview_wrapper_class := TeleviewWrapperClass
                 }=RenderState, Params, Context) ->
    Context1 = z_context:set_language(undefined, Context),

    SrcUrl = z_lib_include:url([ "lib/js/zotonic.teleview.worker.js" ], Context1),
    Base = proplists:get_value(base, Params, <<"cotonic/cotonic-worker.js">>),
    BaseUrl = z_lib_include:url([ Base ], Context1),

    %% Prepare DOM
    CurrentFrame = current_frame(TeleviewId, RendererId, Context),
    Id = z_ids:identifier(),
    TeleviewElementArgs = [{<<"id">>, Id},
                           {<<"data-renderer-id">>, RendererId},
                           {<<"data-teleview-id">>, TeleviewId}],
    TeleviewElementArgs1 = case TeleviewWrapperClass of
                               Class when is_binary(Class) ->
                                   [{<<"class">>, Class} | TeleviewElementArgs];
                               _ ->
                                   TeleviewElementArgs
                           end,
    TeleviewElement = z_tags:render_tag(TeleviewWrapperElement, TeleviewElementArgs1, [ CurrentFrame ]),

    %% Prepare script to start the client side worker which handles
    %% the teleview.
    Args = maps:put(id, Id, RenderState),
    ArgsJSON = z_json:encode(Args),
    Name = proplists:get_value(name, Params, Id),
    Spawn = [ <<"cotonic.spawn_named(\"">>, z_utils:js_escape(Name), "\", \"", SrcUrl, "\", \"", BaseUrl, "\",", ArgsJSON, ");" ],

    Script = z_tags:render_tag(<<"script">>, [],
                               [ <<"cotonic.ready.then(function() {">>, Spawn, <<"});">> ]),

    {ok, [TeleviewElement, Script]}.

% @doc Get the televiews minimum time between keyframes
keyframe_min_time(#{ keyframe_min_time := T }) -> T;
keyframe_min_time(#{ }) -> 0.

% @doc Get the televiews maximum time between keyframes
keyframe_max_time(#{ keyframe_max_time := T }) -> T;
keyframe_max_time(#{ }) -> infinite.

% @doc Get the teleview element wrapper class
teleview_wrapper_class(#{ teleview_wrapper_class := Class }) -> Class;
teleview_wrapper_class(#{ }) -> undefined.

% @doc Get the teleview element wrapper class
teleview_wrapper_element(#{ teleview_wrapper_element := Elt}) -> Elt;
teleview_wrapper_element(#{ }) -> <<"div">>.

%
% @doc Get the teleview element wrapper element 


current_frame(TeleviewId, RendererId, Context) ->
    case z_teleview_state:get_current_frame(TeleviewId, RendererId, Context) of
        #{ current_frame := Frame } ->
            Frame;
        _ ->
            <<>>
    end.

