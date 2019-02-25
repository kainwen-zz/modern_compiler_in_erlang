-module(env).

-export([put/2, contain/2, new/0, lookup/2]).

new() ->
    [].

put(Env, Kvs) ->
    [Kvs|Env].

contain([], _Id) ->
    false;
contain([Kvs|Env], Id) ->
    case lists:keyfind(Id, 1, Kvs) of
        false ->
            contain(Env, Id);
        {Id, _} ->
            true
    end.

lookup([], _) ->
    erlang:error("cannot find key");
lookup([Kvs|Env], Id) ->
    case lists:keyfind(Id, 1, Kvs) of
        false ->
            lookup(Env, Id);
        {Id, V} -> V
    end.
