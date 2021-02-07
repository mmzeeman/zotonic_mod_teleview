%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2021 Maas-Maarten Zeeman
%% @doc TeleView Supervisor.

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

-module(z_teleview_sup).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(supervisor).

%% Supervisor, starts state process and render_diff supervisor.

-export([start_link/3]).
-export([init/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

%%
%% Api
%%

start_link(Id, Args, Context) ->
    supervisor:start_link(
      {via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE,
      [Id, Args, Context]).


%%
%% supervisor callback
%%

init([Id, Args, Context]) ->
    ?DEBUG(start_teleview_sup),

    StateSpec = #{id => z_teleview_state,
                  start => {z_teleview_state, start_link, [Id, self(), Args, Context]},
                  restart => transient,
                  shutdown => 5000,
                  type => worker,
                  modules => dynamic},
    {ok, {
       #{strategy => one_for_all,
         intensity => 20,
         period => 10},
       [StateSpec]
      }
    }.
