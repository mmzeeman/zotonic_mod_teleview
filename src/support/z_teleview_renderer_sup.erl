%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2021 Maas-Maarten Zeeman
%% @doc TeleView renderer supervisor for a single render -> differ pipeline.

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

-module(z_teleview_renderer_sup).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

%%
%% Api
%%

start_link(TeleviewId, RendererId, Args, Context) ->
    supervisor:start_link(
      {via, z_proc, {{?MODULE, TeleviewId, RendererId}, Context}}, ?MODULE,
      [TeleviewId, RendererId, Args, Context]).

%%
%% supervisor callback
%%

init([TeleviewId, RendererId, Args, Context]) ->
    RendererContext = renderer_context(Args, Context),

    DifferSpec = #{id => z_teleview_differ,
                   start => {z_teleview_differ, start_link,
                             [TeleviewId, RendererId, Args, RendererContext]},
                   restart => transient,
                   shutdown => 1000,
                   type => worker,
                   modules => [z_teleview_differ]},

    RenderSpec = #{id => z_teleview_render,
                   start => {z_teleview_render, start_link,
                             [TeleviewId, RendererId, self(), Args, RendererContext]},
                   restart => transient,
                   shutdown => 1000,
                   type => worker,
                   modules => [z_teleview_render]},

    {ok, {
       #{strategy => one_for_all,
         intensity => 1,
         period => 5},
       [DifferSpec, RenderSpec]
      }
    }.

%%
%% Helpers
%%

renderer_context(Args, Context) ->
    case z_notifier:first({teleview_renderer_init, Args}, Context) of
        undefined ->
            z_acl:anondo(Context);
        #context{} = NewContext ->
            ?DEBUG({got_context, Args, z_acl:user(NewContext)}),
            NewContext
    end.

