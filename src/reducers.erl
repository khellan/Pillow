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

-module(reducers).
-export([get_reducer/3]).
-vsn(0.1).

%%--------------------------------------------------------------------
%% Function: sum/2
%% Description: Adds the value for the Key in Tuple to the value for
%%    that Key in Acc
%% Returns: The new version of Acc or error
%%--------------------------------------------------------------------
sum(Tuple, Acc) ->
    case Tuple of
        {struct, Pair} ->
            {value, {<<"key">>, Key}} = lists:keysearch(<<"key">>, 1, Pair),
            {value, {<<"value">>, Value}} = lists:keysearch(<<"value">>, 1, Pair),
            case dict:find(Key, Acc) of
                {ok, OldValue} -> dict:store(Key, OldValue + Value, Acc);
                error -> dict:store(Key, Value, Acc)
            end;
        _Other -> error
    end.

%%--------------------------------------------------------------------
%% Function: get_reducer/3
%% Description: Finds the appropriate reduction function for the view if any
%% Returns: The reduction function of other
%%--------------------------------------------------------------------
get_reducer(Db, Design, View) ->
    case {Db, Design, View} of
        {"userprofiles", "reports", "clicked_recommendations_vendor_timestamps"} -> fun(T, A) -> sum(T,A) end;
        {"userprofiles", "reports", "clicked_recommendations_vendor_totals"} -> fun(T, A) -> sum(T,A) end;
        {"userprofiles", "reports", "purchase_count"} -> fun(T, A) -> sum(T,A) end;
        {"userprofiles", "reports", "recommendations_vendor_timestamps"} -> fun(T, A) -> sum(T,A) end;
        {"userprofiles", "reports", "recommendations_vendor_totals"} -> fun(T, A) -> sum(T,A) end;
        {"userprofiles", "reports", "vendor_timestamps"} -> fun(T, A) -> sum(T,A) end;
        {"userprofiles", "reports", "vendor_totals"} -> fun(T, A) -> sum(T,A) end;
        {"userprofiles", "reports", "vendor_uniques"} -> fun(T, A) -> sum(T,A) end;
        {"userprofiles", "filtering", "brand_totals"} -> fun(T, A) -> sum(T,A) end;
        _Other -> other
    end.
