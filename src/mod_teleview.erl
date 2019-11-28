%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman
%% @doc Provides server rendered live updating views.

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

-module(mod_teleview).

-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-mod_title("TeleView").
-mod_description("Provides server rendered live updating views").
-mod_provides([teleview]).
-mod_depends([base, mod_mqtt]).
-mod_prio(1000).

-behaviour(supervisor).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([start_link/1]).
-export([init/1]).

-export([start_teleview/3]).

-define(SERVER, ?MODULE).


start_link(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    supervisor:start_link(
      {local, z_utils:name_for_site(?SERVER, Context)}, ?MODULE, Args).

start_teleview(Id, Args, Context) ->
    AsyncContext = z_context:prune_for_async(Context),

    TeleViewSpec = #{id => Id,
                     start => {z_teleview_sup, start_link, [Id, Args, AsyncContext]},
                     restart => transient,
                     type => supervisor,
                     intensity => 5000,
                     modules => dynamic},

    case supervisor:start_child(z_utils:name_for_site(?SERVER, Context), TeleViewSpec) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    lager:md([
              {site, z_context:site(Context)},
              {module, ?MODULE}
             ]),
    {ok, {{one_for_one, 20, 10}, []}}.
