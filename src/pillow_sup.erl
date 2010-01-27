-module(pillow_sup).
-behaviour(supervisor).

-export([start_link/0,init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

dispatch_map() ->
    [{["doc", '*'], pillow_router, []},
     {["view", '*'], pillow_reducer, []}].

init_routing_table() ->
    ets:new(routingTable, [set, named_table, public]).

fill_routing_table() ->
    ets:insert(routingTable, {1, "http://localhost:5984/"}),
    ets:insert(routingTable, {2, "http://localhost:5997/"}),
    ok.

%% @doc supervisor callback.
init([]) ->
    init_routing_table(),
    fill_routing_table(),
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
    {ok, {{one_for_one, 10, 10}, Processes}}.
