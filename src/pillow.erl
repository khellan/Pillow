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

-module(pillow).

-export([start/0, stop/0, update_routing_table/0]).

%%--------------------------------------------------------------------
%% EXPORTED FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts Pillow and necessary other stuff
%% Returns: ok if everything starts successfully
%%--------------------------------------------------------------------
start() ->
    pillow_routing_table:start_link(),
    application:start(ibrowse),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(pillow).

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stops Pillow and necessary other stuff
%% Returns: ok if everything stops successfully
%%--------------------------------------------------------------------
stop() ->
    Res = application:stop(pillow),
    application:stop(webmachine),
    application:stop(crypto),
    application:stop(ibrowse),
    pillow_routing_table:stop(),
    Res.

%%--------------------------------------------------------------------
%% Function: update_routing_table/0
%% Description: Purges and reloads the routing table
%% Returns: {upgrade, PreVersion, PostVersion}
%%--------------------------------------------------------------------
update_routing_table() ->
    pillow_routing_table:update_routing_table().
    
%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: ensure_started/1
%% Description: Starts App if it's not started
%% Returns: ok
%%--------------------------------------------------------------------
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
