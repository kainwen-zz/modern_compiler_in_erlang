-module(bst).

-export([empty/0, insert/2, member/2]).

-export_type([tree/0, key/0]).

-type key() :: string().
-type tree() :: leaf
              | {tree,
                 Left::tree(),
                 key(),
                 Right::tree()}.

-spec empty() -> tree().
empty() -> leaf.

-spec insert(key(), tree()) -> tree().
insert(Key, leaf) -> {tree, leaf, Key, leaf};
insert(Key, {tree, Left, K, Right}) when Key < K ->
    {tree,
     insert(Key, Left),
     K,
     Right};
insert(Key, {tree, Left, K, Right}) when Key > K ->
    {tree,
     Left,
     K,
     insert(Key, Right)};
insert(Key, {tree, Left, _K, Right}) ->
    {tree, Left, Key, Right}.
    

-spec member(key(), tree()) -> boolean().
member(_Key, leaf) -> false;
member(Key, {tree, Left, K, _Right}) when Key < K ->
    member(Key, Left);
member(Key, {tree, _Left, K, Right}) when Key > K ->
    member(Key, Right);
member(_Key, {tree, _Left, _K, _Right}) -> true.
