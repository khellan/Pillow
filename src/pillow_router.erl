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

-module(pillow_router).
-export([init/1, to_json/2, content_types_provided/2, allowed_methods/2, receive_data/2]).
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
    Results = get_all_server_results(ReqData),
    {Results, ReqData, Context}.

%%--------------------------------------------------------------------
%% Function: content_types_provided/2
%% Description: Defines a mapping of format to function provided by
%%    this resource
%% Returns: the map
%%--------------------------------------------------------------------
content_types_provided(ReqData, Context) ->
    {[{"text/plain", to_json}], ReqData, Context}.

%%--------------------------------------------------------------------
%% Function: allowed_methods/2
%% Description: Defines the allowed method for this resource
%% Returns: {['GET', 'POST', 'PUT', 'DELETE'], ReqData, Context}
%%--------------------------------------------------------------------
allowed_methods(ReqData, Context) ->
    {['GET', 'POST', 'PUT', 'DELETE'], ReqData, Context}.

%%--------------------------------------------------------------------
%% Function: receive_data/2
%% Description: Returns the result of the request in json form
%% Returns: The result of the request
%%--------------------------------------------------------------------
receive_data(ReqData, Context) ->
    Results = get_all_server_results(ReqData),
    ModReqData = wrq:append_to_response_body(Results, ReqData),
    {Results, ModReqData, Context}.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: make_target_url/2
%% Description: Combines Server and ReqData into the target url
%% Returns: TargetUrl
%%--------------------------------------------------------------------
make_target_url(Server, ReqData) ->
    lists:flatten([Server, wrq:disp_path(ReqData)]).

%%--------------------------------------------------------------------
%% Function: get_single_server_result/2
%% Description: Retrieves result from a single server
%% Returns: The result retrieved from the server
%%--------------------------------------------------------------------
get_single_server_result(Server, ReqData) ->
    Method = wrq:method(ReqData),
    TargetUrl = make_target_url(Server, ReqData),
    io:format("~s: ~s~n", [Method, TargetUrl]),
    {ok, _Code, _Headers, Body} = case Method of
        'GET' ->
            ibrowse:send_req(TargetUrl, [], get);
        'PUT' ->
            Payload = binary_to_list(wrq:req_body(ReqData)),
            ibrowse:send_req(TargetUrl, [], put, Payload);
        _ -> "Better wait"
    end,
    io:format("Body: ~s", [Body]),
    Body.

%%--------------------------------------------------------------------
%% Function: get_all_server_results/1
%% Description: Retrieves result from the correct server based on the
%%    routing table
%% Returns: The result retrieved from the server
%%--------------------------------------------------------------------
get_all_server_results(ReqData) ->
    PathElements = wrq:path_tokens(ReqData),
    {Db, Id} = {lists:nth(1, PathElements), lists:nth(2, PathElements)},
    get_single_server_result(pillow_routing_table:get(pillow_routing_table:hash(Db, Id), pillow_routing_table:init()), ReqData).

