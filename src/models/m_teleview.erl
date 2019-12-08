
-module(m_teleview).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(zotonic_model).

-export([
    m_get/2
]).

%% @doc ...
m_get([get, topics, Label | _Rest], _Context) ->
    ok.


%%
%% Helpers
%%
