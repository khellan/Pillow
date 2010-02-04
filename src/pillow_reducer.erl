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

-module(pillow_reducer).
-export([init/1, to_json/2, content_types_provided/2, get_single_server_result/3, update_view_map/0]).
-include_lib("deps/webmachine/include/webmachine.hrl").

%%--------------------------------------------------------------------
%% EXPORTED FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Nothing yet
%% Returns: {ok, undefined}
%%--------------------------------------------------------------------
init([]) -> {ok, undefined}.

%%--------------------------------------------------------------------
%% Function: to_json/2
%% Description: Returns json formatted reduction result for the request
%% Returns: The result of the request
%%--------------------------------------------------------------------
to_json(ReqData, Context) ->
    Results = mochijson2:encode(get_all_server_results(ReqData)),
%    Results = get_all_server_results(ReqData),
    {Results, ReqData, Context}.

%%--------------------------------------------------------------------
%% Function: content_types_provided/2
%% Description: Defines a mapping of format to function provided by
%%    this resource
%% Returns: the map
%%--------------------------------------------------------------------
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_json}], ReqData, Context}.

%%--------------------------------------------------------------------
%% Function: get_single_server_result/3
%% Description: Retrieves the map reduce result from a single server
%%    and sends the result back to Pid
%% Returns: The result of sending the result back to Pid or raises
%%    an erlang:error with Conneciton Failed on error
%%--------------------------------------------------------------------
get_single_server_result(Server, ReqData, Pid) ->
    TargetPath = make_target_path(Server, ReqData),
    case ibrowse:send_req(TargetPath, [], get) of
        {ok, _Code, _Headers, Body} -> Pid ! {self(), mochijson2:decode(Body)};
        {error, conn_failed} -> erlang:error(["Connection Failed", TargetPath])
    end.

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
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: reduce/2
%% Description: Runs the reduce function, Reducer over all elements in
%%    List
%% Returns: A list of the reduced values
%%--------------------------------------------------------------------
% Simple sum reducer using add_tuple
reduce(List, Reducer) ->
    Result = dict:to_list(lists:foldl(fun(Tuple, Acc) -> Reducer(Tuple, Acc) end, dict:new(), List)),
    lists:map(fun(Tuple) -> {struct, [{<<"key">>, element(1, Tuple)}, {<<"value">>, element(2, Tuple)}]} end, Result).

%%--------------------------------------------------------------------
%% Function: do_reduce/3
%% Description: Finds and returns the correct reducer for Db, Design,
%%    View
%% Returns: The Reducer for the Db, Design, View if none is specified,
%%    fun(List) -> List is returned
%%--------------------------------------------------------------------
do_reduce(Db, Design, View) ->
    case reducers:get_reducer(Db, Design, View) of
        other -> fun(List) -> List end;
        Reducer -> fun(List) -> reduce(List, Reducer) end
    end.

%%--------------------------------------------------------------------
%% Function: make_query_string/1
%% Description: Combines the query string key, value pairs into a
%%    query string
%% Returns: The query string
%%--------------------------------------------------------------------
make_query_string(Params) ->
    lists:map(fun({Key, Value}) -> io_lib:format("~s=~s", [Key, Value]) end, Params).

%%--------------------------------------------------------------------
%% Function: make_target_path/2
%% Description: Creates the correct url to the a single server.
%% Returns: The server url
%%--------------------------------------------------------------------
make_target_path(Server, ReqData) ->
    Path = io_lib:format("~s~s?~s", [Server, wrq:disp_path(ReqData), make_query_string(wrq:req_qs(ReqData))]),
    re:replace(re:replace(Path," ", "%20", [global]), "\"", "%22", [global, {return, list}]).

%%--------------------------------------------------------------------
%% Function: single_serer_result_retriever/2
%% Description: Spawns a get_single_server_result for a single server
%% Returns: The pid of the spawned process
%%--------------------------------------------------------------------
single_server_result_retriever(Server, ReqData) ->
    spawn(?MODULE, get_single_server_result, [Server, ReqData, self()]).

%%--------------------------------------------------------------------
%% Function: get_all_responses/1
%% Description: Waits for response from all servers. Times out after
%%    1 second
%% Returns: A list of all the server responses
%%--------------------------------------------------------------------
get_all_responses([]) -> [];
get_all_responses(Servers) ->
    receive
        {Pid, Response} ->
            NewServers = lists:delete(Pid, Servers),
            lists:flatten([Response, get_all_responses(NewServers)])
        after 5000 -> []
    end.

%%--------------------------------------------------------------------
%% Function: extract_rows/1
%% Description: Recursively extracts the rows of data from the merged
%%    results
%% Returns: A list of the values from the rows fields in the results
%%--------------------------------------------------------------------
extract_rows([]) ->
    [];
extract_rows([Head | Tail]) ->
    case Head of
        {struct, SubSet} ->
            {value, {<<"rows">>, Rows}} = lists:keysearch(<<"rows">>, 1, SubSet),
            [Rows | extract_rows(Tail)];
        _Other -> []
    end.

%%--------------------------------------------------------------------
%% Function: reduce_result/4
%% Description: Reduces the all the result
%% Returns: The reduced result
%%--------------------------------------------------------------------
reduce_result(Db, Design, View, RawResult) ->
    {struct, [{<<"rows">>, (do_reduce(Db, Design, View))(lists:flatten(extract_rows(RawResult)))}]}.
%    [Head | Tail] = RawResult,
%    Head.

%%--------------------------------------------------------------------
%% Function: get_db_design_view/1
%% Description: Extracts DB, Design and View names from the path
%% Returns: {Db, Design, View}
%%--------------------------------------------------------------------
get_db_design_view(ReqData) ->
    PathElements = wrq:path_tokens(ReqData),
    {lists:nth(1, PathElements), lists:nth(3, PathElements), lists:nth(5, PathElements)}.

%%--------------------------------------------------------------------
%% Function: get_all_server_results/1
%% Description: Fans the request out to all the servers and rereduces
%%    their results
%% Returns: The reduced result
%%--------------------------------------------------------------------
get_all_server_results(ReqData) ->
    Servers = lists:map(fun({_, Server}) -> single_server_result_retriever(Server, ReqData) end, pillow_routing_table:to_list(pillow_routing_table:init())),
    {Db, Design, View} = get_db_design_view(ReqData),
    reduce_result(Db, Design, View, get_all_responses(Servers)).
%    io_lib:format("Db: ~s, Design: ~s, View: ~s", [Db, Design, View]).
