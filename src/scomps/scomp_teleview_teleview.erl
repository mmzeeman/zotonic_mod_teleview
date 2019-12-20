%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman
%% @doc Put a teleview on the page.

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


-module(scomp_teleview_teleview).
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

%% Make a place for the view to land and setup the worker which
%% is going to manage the view.

render(Params, Vars, Context) ->
    ?DEBUG({Params, Vars}),

    case ?DEBUG(proplists:get_value(live, Params)) of
        undefined ->
            ?DEBUG("Missing live attribute"),
            {ok, <<>>};
        {LiveType, Attrs} ->
            ?DEBUG({LiveType, Attrs}),
            case z_notifier:first({teleview_live, LiveType, Attrs, Vars}, Context) of
                undefined ->
                    ?DEBUG("No teleview registered"),
                    {ok, <<>>};
                {start_teleview, Details} ->
                    ?DEBUG("No live topic registered"),
                    {ok, <<>>};
                {ok, Topic} ->
                    {ok, <<"<p>Teleview dinges</p>">>}
            end
    end.

