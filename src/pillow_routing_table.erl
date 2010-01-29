-module(pillow_routing_table).
-export([init/0, hash/2, to_list/1, get/2]).

init_routing_table() ->
    dict:new().

fill_routing_table(Dict) ->
    Dict1 = dict:store(1, "http://localhost:5984/", Dict),
    Dict2 = dict:store(2, "http://localhost:5997/", Dict1),
    Dict2.

hash(Db, Id) ->
    Hash = erlang:phash2(Id),
    case Db of
        "userprofiles" -> case Hash of
                X when 0 =< X andalso X < 134217728 -> 1
            end;
        _Other -> 1
    end.
%    round(random:uniform() + 1).

get(Key, Dict) ->
    dict:fetch(Key, Dict).
    
to_list(Dict) ->
    dict:to_list(Dict).
    
init() ->
    Dict = init_routing_table(),
    fill_routing_table(Dict).
