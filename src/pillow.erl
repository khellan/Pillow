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

-export([start/0, stop/0, update_routing_table/0, update_view_map/0, get_version/0]).
-export([reshard/0, flip/0, reshard_and_flip/0]).

%%--------------------------------------------------------------------
%% EXPORTED FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts Pillow and necessary other stuff
%% Returns: ok if everything starts successfully
%%--------------------------------------------------------------------
start() ->
    application:start(ibrowse),
    application:start(couch_config),
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
    pillow_routing_table:stop(),
    application:stop(webmachine),
    application:stop(crypto),
    application:stop(couch_config),
    application:stop(ibrowse),
    Res.

%%--------------------------------------------------------------------
%% Function: update_routing_table/0
%% Description: Purges and reloads the routing table
%% Returns: {upgrade, PreVersion, PostVersion}
%%--------------------------------------------------------------------
update_routing_table() ->
    pillow_routing_table:update_routing_table().
    
%%--------------------------------------------------------------------
%% Function: update_view_map/0
%% Description: Reloads the view map
%% Returns: {upgrade, PreVersion, PostVersion}
%%--------------------------------------------------------------------
update_view_map() ->
    [{attributes, PreAttributes}] = lists:filter(fun(X) -> element(1, X) == attributes end, reducers:module_info()),
    [{vsn, [PreVersion]}] = lists:filter(fun(X) -> element(1, X) == vsn end, PreAttributes),
    code:purge(reducers),
    code:load_file(reducers),
    [{attributes, PostAttributes}] = lists:filter(fun(X) -> element(1, X) == attributes end, reducers:module_info()),
    [{vsn, [PostVersion]}] = lists:filter(fun(X) -> element(1, X) == vsn end, PostAttributes),
    {upgrade, PreVersion, PostVersion}.

%%--------------------------------------------------------------------
%% Function: get_version/0
%% Description: Returns the version number of Pillow
%% Returns: Version number
%%--------------------------------------------------------------------
get_version() ->
    case lists:keysearch(pillow, 1, application:loaded_applications()) of
    {value, {_, _, Vsn}} ->
        Vsn;
    false ->
        "0.0.0"
    end.

%%--------------------------------------------------------------------
%% Function: reshard/0
%% Description: Tells the routing table to reshard
%% Returns: ok
%%--------------------------------------------------------------------
reshard() ->
    pillow_routing_table:reshard(false).

%%--------------------------------------------------------------------
%% Function: reshard/0
%% Description: Tells the routing table to reshard
%% Returns: ok
%%--------------------------------------------------------------------
reshard_and_flip() ->
    pillow_routing_table:reshard(true).

%%--------------------------------------------------------------------
%% Function: flip/0
%% Description: Tells the routing table to flip to the new servers
%% Returns: ok
%%--------------------------------------------------------------------
flip() ->
    pillow_routing_table:flip().

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
