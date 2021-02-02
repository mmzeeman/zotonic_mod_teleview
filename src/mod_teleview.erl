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

-export([
    start_teleview/2
]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    supervisor:start_link(
      {local, z_utils:name_for_site(?SERVER, Context)}, ?MODULE, Args).


% @doc ensure_teleview without giving an explicit Id. The Id will be generated.
start_teleview(Args, Context) ->
    Id = erlang:phash2(Args),
    start_teleview(Id, Args, Context).

% @doc ensure_teleview starts a new teleview with the given Id.
start_teleview(Id, #{ <<"template">> := _Template } = Args, Context) ->
    AsyncContext = z_context:prune_for_async(Context),
    case ?DEBUG(supervisor:start_child(z_utils:name_for_site(?SERVER, Context), [Id, Args, AsyncContext])) of
        {ok, _Pid} ->
            {ok, Id};
        {error, {already_started, _Pid}} -> 
            {ok, Id};
        {error, _}=Error ->
            Error
    end.

%%
%% Supervisor callback
%%


init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    lager:md([
              {site, z_context:site(Context)},
              {module, ?MODULE}
             ]),

    TeleviewSpec = #{id => z_teleview_sup,
                     start => {z_teleview_sup, start_link, []},
                     restart => transient,
                     shutdown => infinity,
                     type => supervisor,
                     modules => dynamic},

    {ok, {
       #{strategy => simple_one_for_one,
         intensity => 20,
         period => 10},
       [TeleviewSpec]
      }
    }.

