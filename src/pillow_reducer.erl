-module(pillow_reducer).
-export([init/1, to_json/2, content_types_provided/2, get_single_server_result/3]).
-include_lib("deps/webmachine/include/webmachine.hrl").

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_json}], ReqData, Context}.
    
init([]) -> {ok, undefined}.

add_tuple(Tuple, Acc) ->
    case Tuple of
        {struct, Pair} ->
            {value, {<<"key">>, Key}} = lists:keysearch(<<"key">>, 1, Pair),
            {value, {<<"value">>, Value}} = lists:keysearch(<<"value">>, 1, Pair),
            case dict:find(Key, Acc) of
                {ok, OldValue} -> dict:store(Key, OldValue + Value, Acc);
                error -> dict:store(Key, Value, Acc)
            end;
        _Other -> error
    end.

sum_reducer(List) ->
    Result = dict:to_list(lists:foldl(fun(Tuple, Acc) -> add_tuple(Tuple, Acc) end, dict:new(), List)),
    lists:map(fun(Tuple) -> {struct, [{<<"key">>, element(1, Tuple)}, {<<"value">>, element(2, Tuple)}]} end, Result).

get_reducers() ->
    Dict1 = dict:store({"userprofiles", "reports", "clicked_recommendations_vendor_timestamps"}, fun(List) -> sum_reducer(List) end, dict:new()),
    Dict2 = dict:store({"userprofiles", "reports", "clicked_recommendations_vendor_totals"}, fun(List) -> sum_reducer(List) end, Dict1),
    Dict3 = dict:store({"userprofiles", "reports", "purchase_count"}, fun(List) -> sum_reducer(List) end, Dict2),
    Dict4 = dict:store({"userprofiles", "reports", "recommendations_vendor_timestamps"}, fun(List) -> sum_reducer(List) end, Dict3),
    Dict5 = dict:store({"userprofiles", "reports", "recommendations_vendor_totals"}, fun(List) -> sum_reducer(List) end, Dict4),
    Dict6 = dict:store({"userprofiles", "reports", "vendor_timestamps"}, fun(List) -> sum_reducer(List) end, Dict5),
    Dict7 = dict:store({"userprofiles", "reports", "vendor_totals"}, fun(List) -> sum_reducer(List) end, Dict6),
    Dict8 = dict:store({"userprofiles", "reports", "vendor_uniques"}, fun(List) -> sum_reducer(List) end, Dict7),
    Dict8.

get_reducer(Db, Design, View) ->
    case dict:find({Db, Design, View}, get_reducers()) of
        {ok, Reducer} ->Reducer;
        error -> fun(List) -> List end
    end.

make_query_string(Params) ->
    lists:map(fun({Key, Value}) -> io_lib:format("~s=~s", [Key, Value]) end, Params).

make_target_path(Server, ReqData) ->
    Path = io_lib:format("~s~s?~s", [Server, wrq:disp_path(ReqData), make_query_string(wrq:req_qs(ReqData))]),
    re:replace(re:replace(Path," ", "%20", [global]), "\"", "%22", [global, {return, list}]).

get_single_server_result(Server, ReqData, Pid) ->
    {ok, _Code, _Headers, Body} = ibrowse:send_req(make_target_path(Server, ReqData), [], get),
    Pid ! {self(), mochijson2:decode(Body)}.

single_server_result_retriever(Server, ReqData) ->
    spawn(?MODULE, get_single_server_result, [Server, ReqData, self()]).

get_all_responses([]) ->
    {};
get_all_responses(Servers) ->
    receive
        {Pid, Response} ->
            NewServers = lists:delete(Pid, Servers),
            lists:flatten([Response, get_all_responses(NewServers)])
        after 5000 -> {}
    end.

reduce([]) ->
    [];
reduce([Head | Tail]) ->
    case Head of
        {struct, SubSet} ->
            {value, {<<"rows">>, Rows}} = lists:keysearch(<<"rows">>, 1, SubSet),
            [Rows | reduce(Tail)];
        _Other -> []
    end.

reduce_result(Db, Design, View, RawResult) ->
    {struct, [{<<"rows">>, (get_reducer(Db, Design, View))(lists:flatten(reduce(RawResult)))}]}.
%    [Head | Tail] = RawResult,
%    Head.

get_db_design_view(ReqData) ->
    PathElements = wrq:path_tokens(ReqData),
    {lists:nth(1, PathElements), lists:nth(3, PathElements), lists:nth(5, PathElements)}.

get_all_server_results(ReqData) ->
    Servers = lists:map(fun({_, Server}) -> single_server_result_retriever(Server, ReqData) end, pillow_routing_table:to_list(pillow_routing_table:init())),
    {Db, Design, View} = get_db_design_view(ReqData),
    reduce_result(Db, Design, View, get_all_responses(Servers)).
%    io_lib:format("Db: ~s, Design: ~s, View: ~s", [Db, Design, View]).

to_json(ReqData, Context) ->
    Results = mochijson2:encode(get_all_server_results(ReqData)),
%    Results = get_all_server_results(ReqData),
    {Results, ReqData, Context}.

