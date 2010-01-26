-module(pillow_router).
-export([init/1, to_html/2]).
-include_lib("deps/webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, Context) ->
    Path = io_lib:format("http://localhost:5984/~s", [wrq:disp_path(ReqData)]),
    {ok, _Code, _Headers, Body} = ibrowse:send_req(Path, [], get),
    {Body, ReqData, Context}.
