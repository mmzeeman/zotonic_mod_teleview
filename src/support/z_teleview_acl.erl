%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2021 Maas-Maarten Zeeman
%% @doc Provides access control to teleview and renderer topics events.

%% Copyright 2021 Maas-Maarten Zeeman 
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

-module(z_teleview_acl).

-export([
    init_table/1,
    cleanup_table/1,
    table/1,

    ensure_teleview_access/2,
    ensure_teleview_access/3,

    ensure_renderer_access/3,
    ensure_renderer_access/4,

    is_view_allowed/3,

    is_event_subscribe_allowed/2,
    is_event_subscribe_allowed/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(DEFAULT_EXPIRE_TIME, 120). % expire time, before the client id is used.

%% Initialise the access control table.
init_table(Context) ->
    TableName = table_name(Context),
    ets:new(TableName, [named_table, set, {keypos, 1},
                        public,
                        {write_concurrency, true}, {read_concurrency, true}]).

%% Cleanup the table, remove all entries from clients who are no longer connected, and
%% expired items.
cleanup_table(Context) ->
    cleanup_expired(Context),
    cleanup_sessions(Context),
    ok.

%% Ensure that the current session has access to the teleview.
ensure_teleview_access(TeleviewId, Context) ->
    ensure_entry(teleview_entry(TeleviewId), Context).

ensure_teleview_access(Key, TeleviewId, Context) ->
    ensure_entry(Key, teleview_entry(TeleviewId), Context).

%% Ensure that the current session has access to the renderer.
ensure_renderer_access(TeleviewId, RendererId, Context) ->
    ensure_entry(renderer_entry(TeleviewId, RendererId), Context).

ensure_renderer_access(Key, TeleviewId, RendererId, Context) ->
    ensure_entry(Key, renderer_entry(TeleviewId, RendererId), Context).


%%
is_view_allowed(TeleviewId, RendererId, Context) ->
    Key = acl_key(Context),
    is_entry_found(Key, renderer_entry(TeleviewId, RendererId), Context).


%% Subscribe on the teleview event topic
is_event_subscribe_allowed(EventTopic, Context) ->
    is_event_subscribe_allowed(acl_key(Context), EventTopic, Context).

%% Subscribe on the teleview event topic
is_event_subscribe_allowed(Key, [TeleviewId, _Event], Context) ->
    is_entry_found(Key, teleview_entry(TeleviewId), Context);
is_event_subscribe_allowed(Key, [TeleviewId, _Event, RendererId | _Rest], Context) ->
    is_entry_found(Key, renderer_entry(TeleviewId, RendererId), Context).

%% Return the contents of the acl table.
table(Context) ->
    ets:tab2list(table_name(Context)).

%%
%% Helpers
%%

teleview_entry(TeleviewId) when is_integer(TeleviewId) ->
    {teleview, TeleviewId};
teleview_entry(TeleviewId) ->
    teleview_entry(z_convert:to_integer(TeleviewId)).

renderer_entry(TeleviewId, RendererId) when is_integer(TeleviewId) andalso is_integer(RendererId) ->
    {renderer, TeleviewId, RendererId};
renderer_entry(TeleviewId, RendererId) ->
    renderer_entry(z_convert:to_integer(TeleviewId), z_convert:to_integer(RendererId)).

ensure_entry(Entry, Context) ->
    ensure_entry(acl_key(Context), Entry, Context).

ensure_entry(undefined, _Entry, _Context) ->
    %% No session, so no entry is needed.
    ok;
ensure_entry(Key, Entry, Context) ->
    Table = table_name(Context),
    case ets:lookup(Table, Key) of
        [] ->
            ExpireToken = expire_token(Context),
            ets:insert(Table, {Key, [Entry], ExpireToken});
        [{Key, Entries, ExpireToken}] ->
            case lists:member(Entry, Entries) of
                true -> ok;
                false ->
                    case expire_token(Context) of
                        ExpireToken ->
                            ets:update_element(Table, Key, [{2, [Entry | Entries]}]);
                        NewExpireToken ->
                            ets:update_element(Table, Key, [{2, [Entry | Entries]},
                                                            {3, NewExpireToken}])
                    end
            end
    end.



%% Return an expire token. When there is no client id, an expire timestamp will be returned.
expire_token(Context) ->
    case z_context:client_id(Context) of
        {ok, Id} ->
            %% The id of the connected client. When the client is no longer connected,
            %% the entry will be removed. 
            {client_id, Id};
        {error, _} ->
            %% Temporary expire token using a timestamp
            {expire, z_utils:now() + ?DEFAULT_EXPIRE_TIME} 
    end.

% Remove expired entries.
cleanup_expired(Context) ->
    Table = table_name(Context),
    Now = z_utils:now(),
    ets:select_delete(Table, [{{'_','_',{expire,'$1'}},[{'<', '$1', Now}], [true]},
                              {{'_','_','_'},[],[false]}]),
    ok.

% Remove dead client entries.
cleanup_sessions(Context) ->
    Table = table_name(Context),

    Sessions = ets:select(Table, [{{'$1','$2',{client_id,'_'}},[],['$_']}]),
    Dead = lists:filter(fun({_,_,{client_id, Id}}) ->
                                not is_client_alive(Id, Context)
                        end,
                        Sessions),
    [ ets:delete(Table, Key) || {Key, _, _} <- Dead ],

    ok.

%% Return true iff the entry is found in the acl table.
is_entry_found(undefined, _Entry, _Context) ->
    false;
is_entry_found(Key, Entry, Context) ->
    Table = table_name(Context),

    case ets:lookup(Table, Key) of
        [] ->
            false;
        [{Key, Entries, ExpireToken}] ->
            case expire_token(Context) of
                ExpireToken -> nop;
                NewExpireToken ->
                    ets:update_element(Table, Key, [{3, NewExpireToken}])
            end,
            lists:member(Entry, Entries)
    end.

table_name(Context) ->
    z_utils:name_for_site(?MODULE, Context).      

acl_key(Context) ->
    case z_context:session_id(Context) of
        {ok, SessionId} -> SessionId;
        {error, no_session} -> undefined
    end.

is_client_alive(ClientId, Context) when is_binary(ClientId) ->
    %% [TODO] add is_client_alive api function with binary parameter to z_mqtt
    case mqtt_sessions_registry:find_session(z_context:site(Context), ClientId) of
        {ok, Pid} when is_pid(Pid) ->
            erlang:is_process_alive(Pid);
        {error, _} ->
            false
    end.

