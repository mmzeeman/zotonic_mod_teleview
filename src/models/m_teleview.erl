
-module(m_teleview).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(zotonic_model).

-export([
    m_get/2
]).

%% @doc ...

m_get([get, topics, Label | Rest], Context) ->
    {ok, {get_topics(Label, Context), Rest}}.

%%
%% Helpers
%%

get_topics(Label, Context) ->
    z_teleview_state:get_topics(Label, Context).

