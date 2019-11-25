%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019 Maas-Maarten Zeeman
%% @doc TeleView Render Diff Supervisor.

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

-module(z_teleview_render_diff_sup).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

start_link(Id, Args, Context) ->
    supervisor:start_link(
      {via, z_proc, {{?MODULE, Id}, Context}}, ?MODULE,
      [Id, Args, Context]).


init([Id, Args, Context]) ->
    {ok, {{one_for_all, 20, 10},
          [
           {z_teleview_render,
            {z_teleview_render, start_link, [Id, Args, Context]},
            permanent, 5000, worker, dynamic},
           {z_teleview_differ,
            {z_teleview_differ, start_link, [Id, Args, Context]},
            permanent, 5000, worker, dynamic}
          ]}}.
