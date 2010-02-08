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

-module(pillow_routing_table).
-behaviour(gen_server).

% gen_server functions
-export([init/1, start_link/0, start/0, stop/0, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% Functions for general use
-export([update_routing_table/0, to_list/0, get_server/2]).

% Artificial export for upgrading
-export([fill_routing_table/0]).

-vsn(0.3).

%%--------------------------------------------------------------------
%% EXPORTED FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the routing table process and links to it. 
%% Returns: {ok, Pid}
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts the routing table process. Use when testing
%% from the shell
%% Returns: {ok, Pid}
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], [{debug, []}]).

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stops the routing table process. Use when testing
%% from the shell
%% Returns:
%%--------------------------------------------------------------------
stop() ->
    catch gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Callback for stop
%% Returns: ok
%%--------------------------------------------------------------------
terminate(_Reason, _RoutingTable) ->
    ok.
    
%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Creates and returns a new routing table
%% Returns: {ok, RoutingTable}
%%--------------------------------------------------------------------
init(_) ->
    {ok, fill_routing_table()}.

%%--------------------------------------------------------------------
%% Function: to_list/0
%% Description: Creates a list of the values in Dict
%% Returns: A list with values from Dict
%%--------------------------------------------------------------------
to_list() ->
    gen_server:call(?MODULE, to_list).

%%--------------------------------------------------------------------
%% Function: get_server/2
%% Description: Retrieves the right server for the Db, Id pair
%% Returns: A server url
%%--------------------------------------------------------------------
get_server(Db, Id) ->
    gen_server:call(?MODULE, {get_server, Db, Id}).

%%--------------------------------------------------------------------
%% Function: update_routing_table/0
%% Description: Purges and reloads the routing table
%% Returns: {upgrade, PreVersion, PostVersion}
%%--------------------------------------------------------------------
update_routing_table() ->
    gen_server:call(?MODULE, update_routing_table).

%%--------------------------------------------------------------------
%% Function: fill_routing_table/1
%% Description: Creates and fills the routing table
%% Returns: The new dict
%%--------------------------------------------------------------------
fill_routing_table() ->
    Dict1 = dict:store(1, "http://localhost:5985/", dict:new()),
    Dict2 = dict:store(2, "http://localhost:5986/", Dict1),
    dict:store(3, "http://localhost:5987/", Dict2).

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handler for gen_server:call
%% Returns: {reply, Message, RoutingTable}
%%--------------------------------------------------------------------
handle_call({get_server, Db, Id}, _From, RoutingTable) ->    
    {reply, get(hash(Db, Id), RoutingTable), RoutingTable};
handle_call(to_list, _From, RoutingTable) ->
    {reply, dict:to_list(RoutingTable), RoutingTable};
handle_call(update_routing_table, _From, _OldRoutingTable) ->
    {upgrade, PreVersion, PostVersion} = reload_routing_table(),
    {reply, {upgrade, PreVersion, PostVersion}, ?MODULE:fill_routing_table()}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handler for gen_server:cast
%% Returns: {stop, normal, RoutingTable}
%%--------------------------------------------------------------------
handle_cast(stop, RoutingTable) ->
    {stop, normal, RoutingTable}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handler for unknown messages
%% Returns: {noreply, RoutingTable}
%%--------------------------------------------------------------------
handle_info(Info, RoutingTable) ->
    io:format("~s got unknown message: ~w~n", [?MODULE, Info]),
    {noreply, RoutingTable}.

%%--------------------------------------------------------------------
%% Function: code_change/3
%% Description: Callback for release upgrade/downgrade
%% Returns: {ok, RoutingTable}
%%--------------------------------------------------------------------
code_change(_OldVsn, _RoutingTable, _Extra) ->
    {ok, fill_routing_table()}.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: hash/2
%% Description: Returns the index into the routing table for the given
%%     Db, Id pair
%% Returns: The new Dict
%%--------------------------------------------------------------------
hash(Db, Id) ->
    Hash = lists:sum(Id) rem 3 + 1,
    case Db of
        "userprofiles" -> Hash;
        _Other -> 1
    end.

%%--------------------------------------------------------------------
%% Function: get/2
%% Description: Retrieves the requested Value from Dict
%% Returns: The Value of Key in Dict
%%--------------------------------------------------------------------
get(Key, Dict) ->
    dict:fetch(Key, Dict).

%%--------------------------------------------------------------------
%% Function: reload_routing_table/0
%% Description: Purges and reloads the routing table
%% Returns: {upgrade, PreVersion, PostVersion}
%%--------------------------------------------------------------------
reload_routing_table() ->
    [{attributes, PreAttributes}] = lists:filter(fun(X) -> element(1, X) == attributes end, pillow_routing_table:module_info()),
    [{vsn, [PreVersion]}] = lists:filter(fun(X) -> element(1, X) == vsn end, PreAttributes),
    code:purge(?MODULE),
    code:load_file(?MODULE),
    [{attributes, PostAttributes}] = lists:filter(fun(X) -> element(1, X) == attributes end, pillow_routing_table:module_info()),
    [{vsn, [PostVersion]}] = lists:filter(fun(X) -> element(1, X) == vsn end, PostAttributes),
    {upgrade, PreVersion, PostVersion}.
