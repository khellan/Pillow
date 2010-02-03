-module(pillow_router).
-export([init/1, to_json/2, content_types_provided/2]).
-include_lib("deps/webmachine/include/webmachine.hrl").

%%--------------------------------------------------------------------
%% Function: content_types_provided/2
%% Description: Defines a mapping of format to function provided byt this resource
%% Returns: the map
%%--------------------------------------------------------------------
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_json}], ReqData, Context}.

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Nothing yet
%% Returns: {ok, undefined}
%%--------------------------------------------------------------------
init([]) -> {ok, undefined}.

make_target_path(Server, ReqData) ->
    lists:flatten([Server, wrq:disp_path(ReqData)]).

%%--------------------------------------------------------------------
%% Function: get_single_server_result/2
%% Description: Retrieves result from a single server
%% Returns: The result retrieved from the server
%%--------------------------------------------------------------------
get_single_server_result(Server, ReqData) ->
    {ok, _Code, _Headers, Body} = ibrowse:send_req(make_target_path(Server, ReqData), [], get),
    mochijson2:decode(Body).

%%--------------------------------------------------------------------
%% Function: get_all_server_results/1
%% Description: Retrieves result from the correct server based on the routing table
%% Returns: The result retrieved from the server
%%--------------------------------------------------------------------
get_all_server_results(ReqData) ->
    PathElements = wrq:path_tokens(ReqData),
    {Db, Id} = {lists:nth(1, PathElements), lists:nth(2, PathElements)},
    get_single_server_result(pillow_routing_table:get(pillow_routing_table:hash(Db, Id), pillow_routing_table:init()), ReqData).

%%--------------------------------------------------------------------
%% Function: to_json/2
%% Description: Returns the result of the request in json form
%% Returns: The result of the request
%%--------------------------------------------------------------------
to_json(ReqData, Context) ->
    Results = mochijson2:encode(get_all_server_results(ReqData)),
    {Results, ReqData, Context}.
