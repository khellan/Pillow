%%%---------------------------------------------------------------------
%%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%%% use this file except in compliance with the License. You may obtain a copy of
%%% the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%%% License for the specific language governing permissions and limitations under
%%% the License.
%%%---------------------------------------------------------------------

-module(etap_t_001).
-export([start/0]).

%%--------------------------------------------------------------------
%% Tests that all modules can be loaded
%%--------------------------------------------------------------------
start() ->
    etap:plan(12),
    etap_can:loaded_ok(pillow, "module 'pillow' loaded"),
    etap_can:loaded_ok(pillow_app, "module 'pillow_app' loaded"),
    etap_can:loaded_ok(pillow_couch, "module 'pillow_couch' loaded"),
    etap_can:loaded_ok(pillow_monitor, "module 'pillow_monitor' loaded"),
    etap_can:loaded_ok(pillow_reducer, "module 'pillow_reducer' loaded"),
    etap_can:loaded_ok(pillow_router, "module 'pillow_router' loaded"),
    etap_can:loaded_ok(pillow_routing_table, "module 'pillow_routing_table' loaded"),
    etap_can:loaded_ok(pillow_status, "module 'pillow_status' loaded"),
    etap_can:loaded_ok(pillow_sup, "module 'pillow_sup' loaded"),
    etap_can:loaded_ok(pillow_util, "module 'pillow_util' loaded"),
    etap_can:loaded_ok(reducers, "module 'reducers' loaded"),
    etap_can:loaded_ok(static_resource, "module 'static_resource' loaded"),
    etap:end_tests().
