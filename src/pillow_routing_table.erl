-module(pillow_routing_table).
-export([init/0, hash/2, to_list/1, get/2]).
-vsn(0.1).

init_routing_table() ->
    dict:new().

fill_routing_table(Dict) ->
    Dict1 = dict:store(1, "http://localhost:5985/", Dict),
    Dict2 = dict:store(2, "http://localhost:5986/", Dict1),
    dict:store(3, "http://localhost:5987/", Dict2).

hash(Db, Id) ->
    Hash = lists:sum(Id) rem 3 + 1,
    case Db of
        "userprofiles" -> Hash; 
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
