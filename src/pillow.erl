-module(pillow).
-export([start/0, stop/0]).

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
