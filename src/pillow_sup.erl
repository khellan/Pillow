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

-module(pillow_sup).
-behaviour(supervisor).

-export([start_link/1, init/1, couch_config_start_link_wrapper/2, start_webmachine/0]).
-export([pillow_routing_table_start_link_wrapper/1, pillow_monitor_start_link_wrapper/1]).

%%--------------------------------------------------------------------
%% EXPORTED FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: couch_config_start_link_wrapper/1
%% Description: Wrapper for keeping couch_config running
%% Returns: ServerRet
%%--------------------------------------------------------------------
couch_config_start_link_wrapper(IniFiles, FirstConfigPid) ->
    case is_process_alive(FirstConfigPid) of
        true ->
            link(FirstConfigPid),
            {ok, FirstConfigPid};
        false -> couch_config:start_link(IniFiles)
    end.

%%--------------------------------------------------------------------
%% Function: pillow_routing_table_start_link_wrapper/1
%% Description: Wrapper for keeping pillow_routing_table running
%% Returns: ServerRet
%%--------------------------------------------------------------------
pillow_routing_table_start_link_wrapper(FirstRoutingPid) ->
    case is_process_alive(FirstRoutingPid) of
        true ->
            link(FirstRoutingPid),
            {ok, FirstRoutingPid};
        false -> pillow_routing_table:start_link()
    end.

%%--------------------------------------------------------------------
%% Function: pillow_monitor_start_link_wrapper/1
%% Description: Wrapper for keeping pillow_monitor running
%% Returns: ServerRet
%%--------------------------------------------------------------------
pillow_monitor_start_link_wrapper(FirstMonitorPid) ->
    case is_process_alive(FirstMonitorPid) of
        true ->
            link(FirstMonitorPid),
            {ok, FirstMonitorPid};
        false -> pillow_monitor:start_link()
    end.

%%--------------------------------------------------------------------
%% Function: start_link/1
%% Description: Starts the supervisor
%% Returns: ServerRet
%%--------------------------------------------------------------------
start_link(IniFiles) ->
	{ok, ConfigPid} = couch_config:start_link(IniFiles),
    {ok, MonitorPid} = pillow_monitor:start_link(),
    {ok, RoutingPid} = pillow_routing_table:start_link(),

    BaseChildSpecs =
    {{one_for_all, 10, 3600},
        [{couch_config,
            {pillow_sup, couch_config_start_link_wrapper, [IniFiles, ConfigPid]},
            permanent,
            brutal_kill,
            worker,
            [couch_config]},
        {webmachine,
            {pillow_sup, start_webmachine, []},
            permanent,
            infinity,
            supervisor,
            [pillow_sup]},
        {pillow_monitor,
            {pillow_sup, pillow_monitor_start_link_wrapper, [MonitorPid]},
            permanent,
            infinity,
            supervisor,
            [pillow_sup]},
        {pillow_routing_table,
            {pillow_sup, pillow_routing_table_start_link_wrapper, [RoutingPid]},
            permanent,
            infinity,
            supervisor,
            [pillow_sup]}
        ]},
    supervisor:start_link({local, ?MODULE}, ?MODULE, BaseChildSpecs).

%%--------------------------------------------------------------------
%% Function: start_webmachine/0
%% Description: Starts webmachine
%% Returns: ServerRet
%%--------------------------------------------------------------------
start_webmachine() ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    WebConfig = [
		 {ip, Ip},
                 {backlog, 1000},
		 {port, 8000},
                 {log_dir, "priv/log"},
		 {dispatch, dispatch_map()}],
    Web = {webmachine_mochiweb,
	   {webmachine_mochiweb, start, [WebConfig]},
	   permanent, 5000, worker, dynamic},
    Processes = [Web],
    supervisor:start_link({local, start_webmachine}, pillow_sup, {{one_for_one, 10, 10}, Processes}).

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Starts webmachine
%% Returns: {ok, {{one_for_one, 10, 10}, Processes}}
%%--------------------------------------------------------------------
init(ChildSpecs) ->
    {ok, ChildSpecs}.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: dispatch_map/0
%% Description: Defines the dispatch map for webmachine
%% Returns: The dispatch map
%%--------------------------------------------------------------------
dispatch_map() ->
    [
        {["static", '*'], static_resource, [couch_config:get("pillow", "document_root")]},
        {['*'], pillow_couch, []}
    ].
