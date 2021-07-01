%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman
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

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

%% Make a place for the view to land and setup the worker which
%% is going to manage the view.

render(Params, _Vars, Context) ->
    Topics = proplists:get_all_values(topic, Params),
    Vary = proplists:get_value(vary, Params),

    Args = maps:from_list(z_utils:prop_delete(topic, z_utils:prop_delete(vary, Params))),
    Args1 = maps:put(topics, Topics, Args),

    case mod_teleview:start_teleview(Args1, Context) of
        {error, _E}=Error ->
            {error, Error};
        {ok, TeleviewId} ->
            {ok, RenderState} = mod_teleview:start_renderer(TeleviewId, Vary, Context),
            render_teleview(RenderState, Params, Context)
    end.

%%
%% Helpers
%%

render_teleview(RenderState, Params, Context) ->
    Id = z_ids:identifier(),

    Context1 = z_context:set_language(undefined, Context),

    RenderState1 = maps:put(uiId, Id, RenderState),

    CurrentFrame = maps:get(current_frame, RenderState1, <<>>),

    Args = case maps:get(min_time, RenderState1, undefined) of
               0 ->
                   maps:without([current_frame, current_frame_sn], RenderState1);
               _ ->
                   RenderState1
           end,
    ArgsJSON = z_json:encode(Args),

    SrcUrl = z_lib_include:url([ "lib/js/zotonic.teleview.worker.js" ], Context1),
    Base = proplists:get_value(base, Params, <<"cotonic/cotonic-worker-bundle.js">>),
    BaseUrl = z_lib_include:url([ Base ], Context1),

    Name = proplists:get_value(name, Params, Id),

    Div = z_tags:render_tag(<<"div">>, [{<<"id">>, Id}], [ CurrentFrame ]),

    Spawn = [ <<"cotonic.spawn_named(\"">>, z_utils:js_escape(Name), "\", \"", SrcUrl, "\", \"", BaseUrl, "\",", ArgsJSON, ");" ],

    Script = z_tags:render_tag(<<"script">>, [],
                               [
                                <<"cotonic.ready.then(function() {">>,
                                    Spawn,
                                <<"});">>
                               ]),

    {ok, [Div, Script]}.

