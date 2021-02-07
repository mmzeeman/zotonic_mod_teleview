%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2021 Maas-Maarten Zeeman
%% @doc TeleView supervisor for a collection of renderers of a single teleview.

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

-module(z_teleview_renderers_sup).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(supervisor).

-export([
    start_link/2
]).

-export([init/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

%%
%% Api
%%

start_link(Id, Context) ->
    ?DEBUG({"start renderers supervisor", Id}),
    supervisor:start_link(
      ?MODULE,
      [Id]
     ).


%%
%% supervisor callback
%%

init([Id]) ->
    {ok, {
       #{strategy => simple_one_for_one,
         intensity => 20,
         period => 10},
       [#{id => z_renderer_sup,
          start => {z_teleview_renderer_sup, start_link, [Id]},
          restart => transient,
          shutdown => infinity, 
          type => supervisor,
          modules => [z_teleview_renderer_sup]}]
      }
    }.

