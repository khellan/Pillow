-module(pillow_reducer).
-export([init/1, to_json/2, content_types_provided/2]).
-include_lib("deps/webmachine/include/webmachine.hrl").

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_json}], ReqData, Context}.
    
init([]) -> {ok, undefined}.

make_query_string(Params) ->
    lists:map(fun({Key, Value}) -> io_lib:format("~s=~s", [Key, Value]) end, Params).

make_target_path(ReqData) ->
    Path = io_lib:format("http://localhost:5984/~s?~s", [wrq:disp_path(ReqData), make_query_string(wrq:req_qs(ReqData))]),
    re:replace(re:replace(Path," ", "%20", [global]), "\"", "%22", [global, {return, list}]).

to_json(ReqData, Context) ->
    {ok, _Code, _Headers, Body} = ibrowse:send_req(make_target_path(ReqData), [], get),
    {Body, ReqData, Context}.
