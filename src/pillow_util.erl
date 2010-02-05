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

-module(pillow_util).
-export([uuid/0]).
-include_lib("deps/webmachine/include/webmachine.hrl").

%%--------------------------------------------------------------------
%% EXPORTED FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: uuid/0
%% Description: Generates a hexadecimal uuid as couchdb does.
%%    (Borrowed from couch_util in couchdb)
%% Returns: Hexadecimal list
%%--------------------------------------------------------------------
uuid() ->
    to_hex(crypto:rand_bytes(16)).

%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: to_hex/1
%% Description: Converts the input binary to a hexadecimal list
%%    (Borrowed from couch_util in couchdb)
%% Returns: Hexadecimal list
%%--------------------------------------------------------------------
to_hex([]) -> [];
to_hex(Bin) when is_binary(Bin) -> to_hex(binary_to_list(Bin));
to_hex([H|T]) -> [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

%%--------------------------------------------------------------------
%% Function: make_target_url/2
%% Description: Converts the given number to an ascii digit
%%    (Borrowed from couch_util in couchdb)
%% Returns: TargetUrl
%%--------------------------------------------------------------------
to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.

