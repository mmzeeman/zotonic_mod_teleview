%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2019-2021 Maas-Maarten Zeeman
%% @doc Provides server rendered live updating views.

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

-module(mod_teleview).

-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-mod_title("TeleView").
-mod_description("Provides server rendered live updating views").
-mod_provides([teleview]).
-mod_depends([base, mod_mqtt, mod_server_storage]).
-mod_prio(1000).

-behaviour(supervisor).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([start_link/1]).
-export([init/1]).

-export([
    observe_acl_is_allowed/2,
    observe_tick_10m/2,

    start_teleview/2,
    stop_teleview/2,

    start_renderer/3,

    ensure_renderer/5,

    render/3,
    render/4,

    teleview_count/1,
    renderer_count/1,

    teleview_id/1,
    renderer_id/2
]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    supervisor:start_link(
      {local, z_utils:name_for_site(?SERVER, Context)}, ?MODULE, Args).

% @doc Generate a teleview id from the provided arguments
teleview_id(Args) ->
    erlang:phash2(Args).

% @doc Generate a renderer id from the teleview_id and the provided arguments
renderer_id(TeleviewId, Args) ->
    erlang:phash2({renderer, TeleviewId, Args}).


% @doc start_teleview without giving an explicit Id. The Id will be generated.
start_teleview(Args, Context) ->
    %% Make an id using the arguments of the teleview. When the topic, or
    %% or anything else is changed, this will result in a new teleview.
    Id = teleview_id(Args),
    start_teleview(Id, Args, Context).

% @doc start_teleview starts a new teleview with the given Id.
start_teleview(Id, #{ template := _Template } = Args, Context) ->
    AsyncContext = z_context:prune_for_scomp(Context),

    case supervisor:start_child(z_utils:name_for_site(?SERVER, Context), [Id, Args, AsyncContext]) of
        {ok, _Pid} ->
            z_teleview_acl:ensure_teleview_access(Id, Context),
            {ok, Id};
        {error, {already_started, _Pid}} -> 
            z_teleview_acl:ensure_teleview_access(Id, Context),
            {ok, Id};
        {error, _}=Error ->
            Error
    end.


% @doc Stop the teleview with the specified id.
stop_teleview(Id, Context) ->
    case z_proc:whereis_name({{z_teleview_sup, Id}, Context}) of
        Pid when is_pid(Pid) ->
            case supervisor:terminate_child(z_utils:name_for_site(?SERVER, Context), Pid) of
                ok -> ok;
                {error, not_found} -> ok
            end;
        _ ->
            ok
    end.

% @doc Start a new renderer belonging to a teleview. The passed args and context are
% used for rendering. The teleview must already be started earlier.
-spec start_renderer(integer(), map(), zotonic:context()) -> {ok, integer()} | {error, _}.
start_renderer(TeleviewId, Args, Context) ->
    case z_teleview_state:start_renderer(TeleviewId, Args, Context) of
        {ok, RendererId} ->
            z_teleview_acl:ensure_renderer_access(TeleviewId, RendererId, Context),
            {ok, RendererId};
        Error ->
            Error 
    end.

% @doc make sure the teleview and renderer are running. Possibly starting new ones 
% with the provided args.
ensure_renderer(TeleviewId, RendererId, Args, Vary, Context) ->
    case start_teleview(TeleviewId, Args, Context) of
        {ok, TeleviewId} ->
            case start_renderer(TeleviewId, Vary, Context) of
                {ok, #{ renderer_id := RendererId, teleview_id := TeleviewId} = RendererState} ->
                    RendererState;
                _ ->
                    {error, renderer_start_problem}
            end;
        _ ->
            {error, teleview_start_problem}
    end.


% @doc Trigger a render of a specific renderer of a teleview.
render(TeleviewId, RendererId, Context) -> 
    z_teleview_render:render(TeleviewId, RendererId, Context).

% @doc Trigger a render, with args, of a specific renderer of a teleview.
render(TeleviewId, RendererId, Args, Context) -> 
    z_teleview_render:render(TeleviewId, RendererId, Args, Context).


% @doc Check if the access to teleview and renderer topics is allowed
observe_acl_is_allowed(#acl_is_allowed{action=subscribe,
                                       object=#acl_mqtt{topic=[<<"model">>, <<"teleview">>, <<"event">> | TeleviewEventTopic]}}, Context) ->
    z_teleview_acl:is_event_subscribe_allowed(TeleviewEventTopic, Context);
observe_acl_is_allowed(#acl_is_allowed{}, _Context) ->
    undefined.

% @doc Cleanup the acl table
observe_tick_10m(tick_10m, Context) ->
    z_teleview_acl:cleanup_table(Context).

% @doc Return the number of televiews
teleview_count(Context) ->
    Counts = supervisor:count_children(z_utils:name_for_site(?SERVER, Context)),
    proplists:get_value(active, Counts).

% @doc Return the number of renderers
renderer_count(Context) ->
    RendererCounts = [ begin 
                           Children = supervisor:which_children(Pid),
                           case lists:keyfind(z_teleview_renderers_sup, 1, Children) of
                                false -> 0;
                                {z_teleview_renderers_sup, RenderersSupPid, _, _} ->
                                   Counts = supervisor:count_children(RenderersSupPid),
                                   proplists:get_value(active, Counts)
                           end
                        end || {_, Pid, supervisor, _} <- supervisor:which_children(z_utils:name_for_site(?SERVER, Context))],

    lists:sum(RendererCounts).


%%
%% Supervisor callback
%%

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    z_context:logger_md(Context),
    z_teleview_state:init_table(Context),
    z_teleview_acl:init_table(Context),

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

