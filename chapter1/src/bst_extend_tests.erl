-module(bst_extend_tests).

-include_lib("eunit/include/eunit.hrl").

bst_extend_test() ->
    T1 = bst_extend:empty(),
    T2 = bst_extend:insert("g", 1, T1),
    T3 = bst_extend:insert("h", 2, T2), 
    T4 = bst_extend:insert("f", 3, T3),
    T5 = bst_extend:insert("i", 4, T4),
    T6 = bst_extend:insert("e", 5, T5),
    T7 = bst_extend:insert("z", 6, T6),

    ?assert(bst_extend:lookup("g", T7) =:= 1),
    ?assert(bst_extend:lookup("h", T7) =:= 2),
    ?assert(bst_extend:lookup("f", T7) =:= 3),
    ?assert(bst_extend:lookup("i", T7) =:= 4),
    ?assert(bst_extend:lookup("e", T7) =:= 5),
    ?assert(bst_extend:lookup("z", T7) =:= 6),
    ?assertError({key_not_found, "a"}, bst_extend:lookup("a", T7)).
    
