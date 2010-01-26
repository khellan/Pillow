-module(pillow_reducer).
-export([init/1, to_json/2, content_types_provided/2]).
-include_lib("webmachine/include/webmachine.hrl").

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.
    
init([]) -> {ok, undefined}.

to_json(ReqData, Context) ->
    Path = wrq:disp_path(ReqData),
    Body = io_lib:format("{""path"": ""~s""}", [Path]),
    {Body, ReqData, Context}.
