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

-module(pillow_couch).
-export([init/1, to_json/2, content_types_provided/2, content_types_accepted/2, allowed_methods/2, receive_data/2, delete_resource/2, process_post/2]).
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
%% Description: Returns the result of the request in json form
%% Returns: The result of the request
%%--------------------------------------------------------------------
to_json(ReqData, Context) ->
    io:format("~s: ~s~n", [wrq:method(ReqData), wrq:raw_path(ReqData)]),
    Id = lists:nth(2, wrq:path_tokens(ReqData)),
    case Id of
        "_design" -> pillow_reducer:to_json(ReqData, Context);
        _ -> pillow_router:to_json(ReqData, Context)
    end.

%%--------------------------------------------------------------------
%% Function: receive_data/2
%% Description: Receives and forwards data
%% Returns: The result of the request
%%--------------------------------------------------------------------
receive_data(ReqData, Context) ->
    io:format("~s: ~s~n", [wrq:method(ReqData), wrq:raw_path(ReqData)]),
    Id = lists:nth(2, wrq:path_tokens(ReqData)),
    case Id of
        "_design" -> error;
        _ -> pillow_router:receive_data(ReqData, Context)
    end.

%%--------------------------------------------------------------------
%% Function: delete_resource/2
%% Description: Forwards the request
%% Returns: The result of the request
%%--------------------------------------------------------------------
delete_resource(ReqData, Context) ->
    io:format("~s: ~s~n", [wrq:method(ReqData), wrq:raw_path(ReqData)]),
    Id = lists:nth(2, wrq:path_tokens(ReqData)),
    case Id of
        "_design" -> error;
        _ -> pillow_router:delete_resource(ReqData, Context)
    end.

process_post(ReqData, Context) ->
    io:format("~s: ~s~n", [wrq:method(ReqData), wrq:raw_path(ReqData)]),
    PathTokens = wrq:path_tokens(ReqData),
    Id = case length(PathTokens) of
        1 -> ok;
        _ ->
            lists:nth(2, PathTokens)
    end,
    case Id of
        ok -> pillow_router:receive_data(ReqData, Context);
        "_design" -> error;
        _ -> error
    end.

%%--------------------------------------------------------------------
%% Function: content_types_provided/2
%% Description: Defines a mapping of format to function provided by
%%    this resource
%% Returns: the map
%%--------------------------------------------------------------------
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_json}, {"text/plain", to_json}, {"application/json", to_json}, {"application/xml", to_json}], ReqData, Context}.

%%--------------------------------------------------------------------
%% Function: content_types_accepted/2
%% Description: Defines a mapping of format to function provided by
%%    this resource
%% Returns: the map
%%--------------------------------------------------------------------
content_types_accepted(ReqData, Context) ->
    {[{"application/json", receive_data}, {"text/plain", receive_data}, {"application/x-www-form-urlencoded", receive_data}], ReqData, Context}.
    
%%--------------------------------------------------------------------
%% Function: allowed_methods/2
%% Description: Defines the allowed method for this resource
%% Returns: {['GET', 'POST', 'PUT', 'DELETE'], ReqData, Context}
%%--------------------------------------------------------------------
allowed_methods(ReqData, Context) ->
    {['GET', 'POST', 'PUT', 'DELETE'], ReqData, Context}.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------
