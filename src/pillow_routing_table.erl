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
-export([update_routing_table/0, to_list/0, get_server/2, reshard/1]).

% Artificial export for upgrading
-export([fill_routing_table/0]).

-vsn(0.4).

%%--------------------------------------------------------------------
%% EXPORTED FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the routing table process and links to it. 
%% Returns: {ok, Pid}
%%--------------------------------------------------------------------
start_link() ->
    application:start(ibrowse),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts the routing table process. Use when testing
%% from the shell
%% Returns: {ok, Pid}
%%--------------------------------------------------------------------
start() ->
    Result = gen_server:start({local, ?MODULE}, ?MODULE, [], [{debug, []}]),
    application:stop(ibrowse),
    Result.

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
%% Function: reshard/1
%% Description: Currently sets up validation filters for resharding the Db
%% Returns: The new dict
%%--------------------------------------------------------------------
reshard(Db) ->
    gen_server:call(?MODULE, {reshard, Db}).

%%--------------------------------------------------------------------
%% Function: fill_routing_table/1
%% Description: Creates and fills the routing table
%% Returns: The new dict
%%--------------------------------------------------------------------
fill_routing_table() ->
%    Dict1 = dict:store(1, "http://localhost:5988/", dict:new()),
%    dict:store(2, "http://localhost:5989/", Dict1).
    dict:store(1, "http://localhost:5984/", dict:new()).

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
    {reply, {upgrade, PreVersion, PostVersion}, ?MODULE:fill_routing_table()};
handle_call({reshard, Db}, _From, RoutingTable) ->
    {reply, execute_reshard(Db, RoutingTable), RoutingTable}.

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
    Hash = lists:sum(Id) rem 1 + 1,
    case Db of
        "userprofiles" -> Hash;
        _Other -> 1
    end.

%%--------------------------------------------------------------------
%% Function: fill_new_routing_table/1
%% Description: Creates and fills the replacement routing table db specific
%% Returns: The new dict
%%--------------------------------------------------------------------
fill_new_routing_table(Db) ->
    Dict1 = dict:store(1, "http://localhost:5988/" ++ Db ++ "/", dict:new()),
    dict:store(2, "http://localhost:5989/" ++ Db ++ "/", Dict1).

%%--------------------------------------------------------------------
%% Function: get_shard_validator/2
%% Description: Creates a javascript shard validator for the given shard number
%% Returns: The new validator in a string
%%--------------------------------------------------------------------
get_shard_validator(Shard, NumShards) ->
    "function(doc, oldDoc, userCtx) {"
    ++ "  var myShardNumber = "
    ++ "  " ++ integer_to_list(Shard) ++ ";"
    ++ "  var shard = 0;"
    ++ "  for (var i = 0; i < doc._id.length; ++i) {"
    ++ "    shard += doc._id.charCodeAt(i);"
    ++ "  }"
    ++ "  shard = (shard % " ++ integer_to_list(NumShards) ++ ") + 1;"
    ++ "  if (shard != myShardNumber) throw({forbidden: 'wrong hash'});"
    ++ "}".

%%--------------------------------------------------------------------
%% Function: store_validator/2
%% Description: Stores Validator in Url
%% Returns: ibrowse response
%%--------------------------------------------------------------------
store_validator(Validator, Url) ->
    DesignDoc = mochijson2:encode(
        {struct, [
            {<<"_id">>, <<"_design/shard">>},
            {<<"validate_doc_update">>, list_to_binary(Validator)}
        ]}),
    io:format("~s: ~s~n", [Url, DesignDoc]),
    ibrowse:send_req(Url, [], post, DesignDoc).
%    {ok, "201", "Headers", "Response"}.


%%--------------------------------------------------------------------
%% Function: create_shard_validator/2
%% Description: creates sharding validators for the given shard and the rest of the Dict
%% Returns: ok or error
%%--------------------------------------------------------------------
create_shard_validator(Shard, Dict) ->
    NumShards = dict:size(Dict),
    Validator = get_shard_validator(Shard, NumShards),
    Result = case dict:find(Shard, Dict) of
        {ok, Url} -> store_validator(Validator, Url);
        _ -> error
    end,
    case Result of
        {ok, "201", _Headers, _Response} ->
            case NumShards of
                Shard -> ok;
                _ -> create_shard_validator(Shard + 1, Dict)
            end;
        _ -> error
    end.

%%--------------------------------------------------------------------
%% Function: create_shard_validators/1
%% Description: creates sharding validators for all servers in Dict
%% Returns: ok or error
%%--------------------------------------------------------------------
create_shard_validators(Dict) ->
    create_shard_validator(1, Dict).

%%--------------------------------------------------------------------
%% Function: init_replication/2
%% Description: Sets up a continuous pull replication
%% Returns: The ibrowse response
%%--------------------------------------------------------------------
init_replication(Source, Target) ->
    Url = Source ++ "_replicate",
    Db = lists:sublist(string:tokens(Target, "/"), 3, 1),
    Message = mochijson2:encode(
        {struct, [
            {<<"source">>, list_to_binary(Db)},
            {<<"target">>, list_to_binary(Target)},
            {<<"continuous">>, true}
        ]}
    ),
    io:format("~s: ~s~n", [Url, Message]),
    ibrowse:send_req(Url, [], post, Message).
%    {ok, "202", "Headers", "Response"}.

%%--------------------------------------------------------------------
%% Function: init_all_replication/4
%% Description: Sets up a continuous push replication for all shards
%% Returns: ok or error depending on result
%%--------------------------------------------------------------------
init_all_replication(SourceShard, TargetShard, SourceDict, TargetDict) ->
    CurrentResult = case dict:find(SourceShard, SourceDict) of
        {ok, SourceUrl} ->
            case dict:find(TargetShard, TargetDict) of
                {ok, TargetUrl} -> init_replication(SourceUrl, TargetUrl);
                _ ->
                    io:format("Tried to find ~i of ~i target shards", [TargetShard, dict:size(TargetDict)]),
                    error
            end;
        _ ->
            io:format("Tried to find ~i of ~i source shards", [SourceShard, dict:size(SourceDict)]),
            error
    end,
    NextResult = case CurrentResult of
        {ok, "202", _Headers1, _Response1} ->
            case dict:size(TargetDict) of
                TargetShard -> ok;
                _ -> init_all_replication(SourceShard, TargetShard + 1, SourceDict, TargetDict)
            end
    end,
    case NextResult of
        ok ->
            case dict:size(SourceDict) of
                SourceShard -> ok;
                _ -> init_all_replication(SourceShard + 1, TargetShard, SourceDict, TargetDict)
            end
    end.

%%--------------------------------------------------------------------
%% Function: execute_reshard/2
%% Description: Currently sets up validation filters for resharding the Db
%% Returns: ok or error depending on result
%%--------------------------------------------------------------------
execute_reshard(Db, RoutingTable) ->
    TargetDict = fill_new_routing_table(Db),
    create_shard_validators(TargetDict),
    init_all_replication(1, 1, RoutingTable, TargetDict).
    
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
