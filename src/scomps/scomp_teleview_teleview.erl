%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2024 Maas-Maarten Zeeman
%% @doc Put a teleview on the page.

%% Copyright 2019-2024 Maas-Maarten Zeeman
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

-define(DEFAULT_KEYFRAME_MIN_TIME, 0).
-define(DEFAULT_KEYFRAME_MAX_TIME, infinite).

vary(_Params, _Context) -> nocache.

%% Make a place for the view to land and setup the worker which
%% is going to manage the view.

render(Params, _Vars, Context) ->
    Topics = proplists:get_all_values(topic, Params),
    RendererArgs = proplists:get_value(vary, Params, #{}),
    Args = maps:without([topic, vary], maps:from_list(Params)),

    TeleviewArgs = maps:put(topics, Topics, Args),

    case ensure_renderer(TeleviewArgs, RendererArgs, Context) of
        {ok, TeleviewId, RendererId} ->
            Pickle = z_utils:pickle(#{ teleview_id => TeleviewId,
                                       renderer_id => RendererId, 
                                       teleview_args => TeleviewArgs,
                                       renderer_args => RendererArgs }, Context),
            render_teleview(#{ teleview_id => TeleviewId,
                               renderer_id => RendererId,
                               teleview_wrapper_element => teleview_wrapper_element(TeleviewArgs),
                               teleview_wrapper_class => teleview_wrapper_class(TeleviewArgs),
                               keyframe_min_time => keyframe_min_time(TeleviewArgs),
                               keyframe_max_time => keyframe_max_time(TeleviewArgs),
                               pickle => Pickle
                             },
                            Params,
                            Context);
        {error, _E}=Error ->
            Error
    end.

%%
%% Helpers
%%

% @doc Make sure the teleview renderer is started.
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
                   teleview_wrapper_class := TeleviewWrapperClass,
                   pickle := Pickle
                 }=RenderState, Params, Context) ->
    Context1 = z_context:set_language(undefined, Context),

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
    {Sts, CurrentFrame, SN} = current_frame(TeleviewId, RendererId, Pickle, Context1),
    TeleviewElement = z_tags:render_tag(TeleviewWrapperElement, TeleviewElementArgs1, [ CurrentFrame ]),
    Script = render_script(Id, Params, RenderState#{ renderer_sts => Sts, current_frame_sn => SN }, Context1),

    {ok, [TeleviewElement, {javascript, Script}]}.

% @doc Render the script which starts the teleview worker.
render_script(Id, Params, RenderState, Context) ->
    SrcUrl = z_lib_include:url([ <<"lib/js/zotonic.teleview.worker.js">> ], Context),
    Base = proplists:get_value(base, Params, <<"cotonic/cotonic-worker.js">>),
    BaseUrl = z_lib_include:url([ Base ], Context),
    Name = proplists:get_value(teleview_worker_name, Params, Id),
    Args = z_json:encode(RenderState#{ id => Id }),
    Spawn = [ <<"cotonic.spawn_named(\"">>, z_utils:js_escape(Name), "\", \"", SrcUrl, "\", \"", BaseUrl, "\", ", Args, ");" ],

    % Zotonic page init can happen before cotonic is ready.
    % NOTE: change when zotonic page init called after cotonic is ready.
    iolist_to_binary([ <<"cotonic.ready.then(() => {">>, Spawn, <<"});">> ]).


% @doc Get the televiews minimum time between keyframes
keyframe_min_time(#{ keyframe_min_time := Time }) when is_integer(Time) andalso Time >= 0 ->
    Time;
keyframe_min_time(#{ keyframe_min_time := Time }) ->
    ?LOG_WARNING(#{ text => "Teleview keyframe_min_time has wrong value (integer >= 0). Using default",
                    value => Time,
                    default => ?DEFAULT_KEYFRAME_MIN_TIME }),
    ?DEFAULT_KEYFRAME_MIN_TIME;
keyframe_min_time(#{ }) ->
    ?DEFAULT_KEYFRAME_MIN_TIME.

% @doc Get the televiews maximum time between keyframes
keyframe_max_time(#{ keyframe_max_time := infinite }) ->
    infinite;
keyframe_max_time(#{ keyframe_max_time := Time }) when is_integer(Time) andalso Time >= 0 ->
    Time;
keyframe_max_time(#{ keyframe_max_time := Time }) ->
    ?LOG_WARNING(#{ text => "Teleview keyframe_max_time has wrong value (infinite | integer >= 0). Using default",
                    value => Time,
                    default => ?DEFAULT_KEYFRAME_MAX_TIME }),
    ?DEFAULT_KEYFRAME_MAX_TIME;
keyframe_max_time(#{ }) ->
    ?DEFAULT_KEYFRAME_MAX_TIME.

% @doc Get the teleview element wrapper class
teleview_wrapper_class(#{ teleview_wrapper_class := Class }) ->
    Class;
teleview_wrapper_class(#{ }) ->
    undefined.

% @doc Get the teleview element wrapper class
teleview_wrapper_element(#{ teleview_wrapper_element := Elt}) ->
    Elt;
teleview_wrapper_element(#{ }) ->
    <<"div">>.

%
% @doc Get the teleview element wrapper element 
current_frame(TeleviewId, RendererId, Pickle, Context) ->
    case z_teleview_state:get_current_frame(TeleviewId, RendererId, Pickle, Context) of
        #{ sts := Sts, current_frame := Frame, current_frame_sn := SN } ->
            {Sts, Frame, SN};
        _ ->
            {erlang:monotonic_time(millisecond), <<>>, 0}
    end.

