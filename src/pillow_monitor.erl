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

-module(pillow_monitor).
-behaviour(gen_server).

% gen_server functions
-export([init/1, start_link/0, stop/0, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% Functions for general use
-export([add_status/2, get_status/1, update_status/2]).

-vsn(0.1).

%%--------------------------------------------------------------------
%% EXPORTED FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the routing table process and links to it.
%% Returns: {ok, Pid}
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stops the routing table process. Use when testing
%% from the shell
%% Returns:
%%--------------------------------------------------------------------
stop() ->
    catch gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Callback for stop
%% Returns: ok
%%--------------------------------------------------------------------
terminate(_Reason, _RoutingTable) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Creates and returns a new routing table
%% Returns: {ok, RoutingTable}
%%--------------------------------------------------------------------
init(_) ->
    {ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: get_status/1
%% Description: Retrieves the given status value
%% Returns: The status value
%%--------------------------------------------------------------------
get_status(Key) ->
    gen_server:call(?MODULE, {get_status, Key}).

%%--------------------------------------------------------------------
%% Function: add_status/2
%% Description: Adds the given status value
%% Returns: ok
%%--------------------------------------------------------------------
add_status(Key, Value) ->
    gen_server:call(?MODULE, {add_status, Key, Value}).

%%--------------------------------------------------------------------
%% Function: update_status/2
%% Description: Updates the given status value providing it exists
%% Returns: ok
%%--------------------------------------------------------------------
update_status(Key, Value) ->
    gen_server:call(?MODULE, {update_status, Key, Value}).

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handler for gen_server:call
%% Returns: {reply, Message, Status}
%%--------------------------------------------------------------------
handle_call(get_status, _From, Status) ->
    {reply, Status, Status};
handle_call({add_status, Key, Value}, From, Status) ->
    NewStatus = dict:store(Key, {From, Value}, Status),
    {reply, ok, NewStatus};
handle_call({update_status, Key, Value}, From, Status) ->
    NewStatus = case dict:find(Key, Status) of
        {ok, ValueTuple} ->
            {Owner, _Value} = ValueTuple,
%            if
%                From == Owner ->
                    dict:store(Key, {Owner, Value}, Status)
%            end
    end,
    {reply, ok, NewStatus};
handle_call({get_status, Key}, _From, Status) ->
    case dict:find(Key, Status) of
        {ok, ValueTuple} ->
            {_Owner, Value} = ValueTuple,
            {reply, Value, Status};
        error -> {reply, error, Status}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handler for gen_server:cast
%% Returns: {stop, normal, Status}
%%--------------------------------------------------------------------
handle_cast(stop, Status) ->
    {stop, normal, Status}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handler for unknown messages
%% Returns: {noreply, Status}
%%--------------------------------------------------------------------
handle_info(Info, Status) ->
    io:format("~s got unknown message: ~w~n", [?MODULE, Info]),
    {noreply, Status}.

%%--------------------------------------------------------------------
%% Function: code_change/3
%% Description: Callback for release upgrade/downgrade
%% Returns: {ok, RoutingTable}
%%--------------------------------------------------------------------
code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

