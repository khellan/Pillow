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

-module(pillow_routing_table).
-export([init/0, hash/2, to_list/1, get_server/2]).
-vsn(0.2).

%%--------------------------------------------------------------------
%% EXPORTED FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: init/0
%% Description: Creates and returns a new routing table
%% Returns: The new routing table
%%--------------------------------------------------------------------
init() ->
    fill_routing_table().

%%--------------------------------------------------------------------
%% Function: hash/2
%% Description: Returns the index into the routing table for the given
%%     Db, Id pair
%% Returns: The new Dict
%%--------------------------------------------------------------------
hash(Db, Id) ->
    Hash = lists:sum(Id) rem 3 + 1,
    case Db of
        "userprofiles" -> Hash; 
        _Other -> 1
    end.

%%--------------------------------------------------------------------
%% Function: to_list/1
%% Description: Creates a list of the values in Dict
%% Returns: A list with values from Dict
%%--------------------------------------------------------------------
to_list(Dict) ->
    dict:to_list(Dict).

%%--------------------------------------------------------------------
%% Function: get_server/2
%% Description: Retrieves the right server for the Db, Id pair
%% Returns: A server url
%%--------------------------------------------------------------------
get_server(Db, Id) ->
    get(hash(Db, Id), pillow_routing_table:init()).

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: get/2
%% Description: Retrieves the requested Value from Dict
%% Returns: The Value of Key in Dict
%%--------------------------------------------------------------------
get(Key, Dict) ->
    dict:fetch(Key, Dict).

%%--------------------------------------------------------------------
%% Function: fill_routing_table/1
%% Description: Creates and fills the routing table
%% Returns: The new dict
%%--------------------------------------------------------------------
fill_routing_table() ->
    Dict1 = dict:store(1, "http://localhost:5985/", dict:new()),
    Dict2 = dict:store(2, "http://localhost:5986/", Dict1),
    dict:store(3, "http://localhost:5987/", Dict2).
