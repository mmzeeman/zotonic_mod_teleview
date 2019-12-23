%%
%% [TODO] copyright
%%

-module(m_teleview).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(zotonic_model).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    m_get/3
]).

%% @doc ...

m_get([topics, Label | Rest], _Msg, Context) ->
    {ok, {get_topics(Label, Context), Rest}};
m_get(V, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, V]),
    {error, unknown_path}.


%%
%% Helpers
%%

get_topics(Label, Context) ->
    ?DEBUG({get_topics, Label}),
    z_teleview_state:get_topics(Label, Context).

