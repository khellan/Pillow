-export([ping/2]).

-include_lib("../deps/webmachine/wm_reqdata.hrl").

ping(ReqData, State) ->
    {pong, ReqData, State}.


