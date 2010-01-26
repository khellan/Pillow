%% @author Knut O. Hellan <knut.hellan@gmail.com>

%% @doc Callbacks for the Pillow application.

-module(pillow_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for Pillow.
start(_Type, _StartArgs) ->
    pillow_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for Pillow.
stop(_State) ->
    ok.
