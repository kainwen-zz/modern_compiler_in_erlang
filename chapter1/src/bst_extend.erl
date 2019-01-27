-module(bst_extend).

-type key() :: string().
-type tree(Value) :: leaf
                   | {tree,
                      Left::tree(Value),
                      {key(), Value},
                      Right::tree(Value)}.

-export_type([tree/1, key/0]).
-export([empty/0, insert/3, lookup/2]).

empty() -> leaf.

-spec insert(key(), Value, tree(Value)) -> tree(Value).
insert(Key, Value, leaf) ->
    {tree,
     leaf,
     {Key, Value},
     leaf};
insert(Key, Value, {tree, Left, {K, V}, Right}) when Key < K ->
    {tree,
     insert(Key, Value, Left),
     {K, V},
     Right};
insert(Key, Value, {tree, Left, {K, V}, Right}) when Key > K ->
    {tree,
     Left,
     {K, V},
     insert(Key, Value, Right)};
insert(Key, Value, {tree, Left, _, Right}) ->
    {tree,
     Left,
     {Key, Value},
     Right}.

-spec lookup(key(), tree(Value)) -> Value.
lookup(Key, leaf) -> erlang:error({key_not_found, Key});
lookup(Key, {tree, Left, {K, _V}, _Right}) when Key < K ->
    lookup(Key, Left);
lookup(Key, {tree, _Left, {K, _V}, Right}) when Key > K ->
    lookup(Key, Right);
lookup(_Key, {tree, _Left, {_K, V}, _Right}) -> V.
