-module(pillow).
-export([start/0, stop/0, update_routing_table/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start() -> ok
%% @doc Start the Pillow server.
start() ->
    application:start(ibrowse),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(pillow).

%% @spec stop() -> ok
%% @doc Stop the Pillow server.
stop() ->
    Res = application:stop(pillow),
    application:stop(webmachine),
    application:stop(crypto),
    application:stop(ibrowse),
    Res.

%%--------------------------------------------------------------------
%% Function: update_routing_table/0
%% Description: Retrieves result from a single server
%% Returns: The result retrieved from the server
%%--------------------------------------------------------------------
update_routing_table() ->
    [{attributes, PreAttributes}] = lists:filter(fun(X) -> element(1, X) == attributes end, pillow_routing_table:module_info()),
    [{vsn, [PreVersion]}] = lists:filter(fun(X) -> element(1, X) == vsn end, PreAttributes),
    code:purge(pillow_routing_table),
    code:load_file(pillow_routing_table),
    [{attributes, PostAttributes}] = lists:filter(fun(X) -> element(1, X) == attributes end, pillow_routing_table:module_info()),
    [{vsn, [PostVersion]}] = lists:filter(fun(X) -> element(1, X) == vsn end, PostAttributes),
    {upgrade, PreVersion, PostVersion}.

