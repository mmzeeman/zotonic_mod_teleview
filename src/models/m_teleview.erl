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
    m_get/3
]).

%% @doc ...

m_get([TeleviewId, RendererId, keyframe | Rest], _Msg, Context) ->
    {ok, {todo, Rest}};

m_get([TeleviewId, RendererId, current | Rest], _Msg, Context) ->
    {ok, {todo, Rest}};

m_get([TeleviewId, topics, Label | Rest], _Msg, Context) ->
    {ok, {todo, Rest}};

m_get(V, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, V]),
    {error, unknown_path}.


%%
%% Helpers
%%

get_topics(Label, Context) ->
    ?DEBUG({get_topics, Label}),
    z_teleview_state:get_topics(Label, Context).

