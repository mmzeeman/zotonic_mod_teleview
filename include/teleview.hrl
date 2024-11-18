%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2024 Maas-Maarten Zeeman
%% @doc Teleview record definitions

%% Copyright 2024 Maas-Maarten Zeeman 
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

-record(teleview_init, {
   args :: map()
}).

-record(teleview_renderer_init, {
   args :: map()
}).

-record(teleview_render, {
    id :: mod_teleview:id(),
    msg :: term(),
    args :: map()
}).

-record(teleview_post, {
    id :: mod_teleview:id(),
    path :: mqtt_sessions:topic(),
    msg :: mqtt_packet_map:mqtt_packet(),
    args :: map()
}).
