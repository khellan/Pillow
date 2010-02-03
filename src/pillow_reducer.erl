-module(pillow_reducer).
-export([init/1, to_json/2, content_types_provided/2, get_single_server_result/3]).
-include_lib("deps/webmachine/include/webmachine.hrl").

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_json}], ReqData, Context}.
    
init([]) -> {ok, undefined}.

% Simple sum reducer using add_tuple
reduce(List, Reducer) ->
    Result = dict:to_list(lists:foldl(fun(Tuple, Acc) -> Reducer(Tuple, Acc) end, dict:new(), List)),
    lists:map(fun(Tuple) -> {struct, [{<<"key">>, element(1, Tuple)}, {<<"value">>, element(2, Tuple)}]} end, Result).

%
do_reduce(Db, Design, View) ->
    case reducers:get_reducer(Db, Design, View) of
        other -> fun(List) -> List end;
        Reducer -> fun(List) -> reduce(List, Reducer) end
    end.
    
% Combines the query string key, value pairs into a query string
make_query_string(Params) ->
    lists:map(fun({Key, Value}) -> io_lib:format("~s=~s", [Key, Value]) end, Params).

% Creates the correct url to the a single server.
make_target_path(Server, ReqData) ->
    Path = io_lib:format("~s~s?~s", [Server, wrq:disp_path(ReqData), make_query_string(wrq:req_qs(ReqData))]),
    re:replace(re:replace(Path," ", "%20", [global]), "\"", "%22", [global, {return, list}]).

% Retrieves the map reduce result from a single server
get_single_server_result(Server, ReqData, Pid) ->
    TargetPath = make_target_path(Server, ReqData),
    case ibrowse:send_req(TargetPath, [], get) of
        {ok, _Code, _Headers, Body} -> Pid ! {self(), mochijson2:decode(Body)};
        {error, conn_failed} -> erlang:error(["Connection Failed", TargetPath])
    end.

% Spawns a get_single_server_result for a single server
single_server_result_retriever(Server, ReqData) ->
    spawn(?MODULE, get_single_server_result, [Server, ReqData, self()]).

% Waits for response from all servers. Times out after 1 second
get_all_responses([]) ->
    {};
get_all_responses(Servers) ->
    receive
        {Pid, Response} ->
            NewServers = lists:delete(Pid, Servers),
            lists:flatten([Response, get_all_responses(NewServers)])
        after 1000 -> {}
    end.

% Recursively extracts the rows of data from the merged results
extract_rows([]) ->
    [];
extract_rows([Head | Tail]) ->
    case Head of
        {struct, SubSet} ->
            {value, {<<"rows">>, Rows}} = lists:keysearch(<<"rows">>, 1, SubSet),
            [Rows | extract_rows(Tail)];
        _Other -> []
    end.

% Applies the correct reducer to the merged result
reduce_result(Db, Design, View, RawResult) ->
    {struct, [{<<"rows">>, (do_reduce(Db, Design, View))(lists:flatten(extract_rows(RawResult)))}]}.
%    [Head | Tail] = RawResult,
%    Head.

% Extracts DB, Design and View names from the path
get_db_design_view(ReqData) ->
    PathElements = wrq:path_tokens(ReqData),
    {lists:nth(1, PathElements), lists:nth(3, PathElements), lists:nth(5, PathElements)}.

% Fans the request out to all the servers and rereduces their results
get_all_server_results(ReqData) ->
    Servers = lists:map(fun({_, Server}) -> single_server_result_retriever(Server, ReqData) end, pillow_routing_table:to_list(pillow_routing_table:init())),
    {Db, Design, View} = get_db_design_view(ReqData),
    reduce_result(Db, Design, View, get_all_responses(Servers)).
%    io_lib:format("Db: ~s, Design: ~s, View: ~s", [Db, Design, View]).

%% Returns json formatted reduction result for the request
to_json(ReqData, Context) ->
    Results = mochijson2:encode(get_all_server_results(ReqData)),
%    Results = get_all_server_results(ReqData),
    {Results, ReqData, Context}.

