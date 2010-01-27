-module(pillow_router).
-export([init/1, to_json/2, content_types_provided/2]).
-include_lib("deps/webmachine/include/webmachine.hrl").

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_json}], ReqData, Context}.

init([]) -> {ok, undefined}.

make_target_path(Server, ReqData) ->
    lists:flatten([Server, wrq:disp_path(ReqData)]).

get_single_server_result(Server, ReqData) ->
    {ok, _Code, _Headers, Body} = ibrowse:send_req(make_target_path(Server, ReqData), [], get),
    mochijson2:decode(Body).

get_all_server_results(ReqData) ->
    get_single_server_result(pillow_routing_table:get(pillow_routing_table:hash("a"), pillow_routing_table:init()), ReqData).

to_json(ReqData, Context) ->
    Results = mochijson2:encode(get_all_server_results(ReqData)),
    {Results, ReqData, Context}.
