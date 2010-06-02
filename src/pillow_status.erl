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

-module(pillow_status).

-export([init/1, to_json/2, to_html/2]).

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
%% Description: Returns json formatted overview of Pillow status
%% Returns: The result of the request
%%--------------------------------------------------------------------
to_json(ReqData, Context) ->
    {mochijson2:encode(json_prepare_status(get_status())), ReqData, Context}.

%%--------------------------------------------------------------------
%% Function: to_html/2
%% Description: Returns html formatted overview of Pillow status
%% Returns: The result of the request
%%--------------------------------------------------------------------
to_html(ReqData, Context) ->
    {html_encode(get_status()), ReqData, Context}.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: html_encode/1
%% Description: Creates html status from the input information
%% Returns: HTML status information
%%--------------------------------------------------------------------
html_encode({Version, ReshardStatus, {CurrentServers, NewServers}}) ->
    "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>"
    ++ "<html>"
    ++ "<head><title>Pillow Status</title>"
    ++ "<meta http-equiv='refresh' content='5' />"
    ++ "<meta http-equiv='Content-Type' content='text/html;charset=utf-8'></head>"
    ++ "<body><h1>Pillow " ++ Version ++ " Status</h1>"
    ++ "<div class='reshard_status'>Servers are " ++ io_lib:format("~s", [ReshardStatus]) ++ "</div>"
    ++ "<div class='current_servers'><h2>Current Servers</h2><ul>"
    ++ lists:map(fun({_, Server}) -> "<li>" ++ Server ++ "</li>" end, CurrentServers)
    ++ "</ul></div>"
    ++ "<div class='new_servers'><h2>New Servers</h2><ul>"
    ++ lists:map(fun({_, Server}) -> "<li>" ++ Server ++ "</li>" end, NewServers)
    ++ "</ul></div></body></html>".

json_prepare_status({Version, Servers}) ->
    {struct, [{version, Version}, {servers, lists:map(fun({_, Server}) -> Server end, Servers)}]}.

%%--------------------------------------------------------------------
%% Function: get_status/1
%% Description: Retrieves raw status information
%% Returns: Status information
%%--------------------------------------------------------------------
get_status() ->
    {pillow:get_version(), pillow_monitor:get_status(pillow_reshard_status), pillow_routing_table:get_status()}.
