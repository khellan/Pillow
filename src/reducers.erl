-module(reducers).
-export([get_reducer/3]).

% This is the actual doer part of sum reducer
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

% Retrieves the specified reducer
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


