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
            render_teleview(RenderState, Context)
    end.

%%
%% Helpers
%%

render_teleview(RenderState, Context) ->
    Id = z_ids:identifier(),

    RenderState1 = maps:put(uiId, Id, RenderState),

    CurrentFrame = maps:get(current_frame, RenderState1, <<>>),

    JSArgs = case maps:get(min_time, RenderState1, undefined) of
                 0 ->
                     Opts = maps:without([current_frame, current_frame_sn], RenderState1),
                     z_utils:js_object(maps:to_list(Opts), Context);
                 _ ->
                     z_utils:js_object(maps:to_list(RenderState1), Context)
             end,

    Div = z_tags:render_tag(<<"div">>, [{<<"id">>, Id}], [ CurrentFrame ]),

    %% TODO: initialization needs own model/namespace
    Script = z_tags:render_tag(<<"script">>, [], [
                                                  <<"cotonic.ready.then(function() {">>,
                                                  <<"initTeleviewer(">>, JSArgs, <<");">>, 
                                                  <<"});">>
                                                 ]),

    {ok, [Div, Script]}.

