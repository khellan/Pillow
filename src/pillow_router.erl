-module(pillow_router).
-export([init/1, to_html/2]).
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, Context) -> {"<html><head><title>WebMachine</title></head><body><h1>WebMachine FTW!</h1></body></html>", ReqData, Context}.
