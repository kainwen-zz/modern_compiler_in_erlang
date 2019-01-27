-module(bst_tests).

-include_lib("eunit/include/eunit.hrl").

bst_test() ->
    T1 = bst:empty(),
    T2 = bst:insert("g", T1),
    T3 = bst:insert("h", T2), 
    T4 = bst:insert("f", T3),
    T5 = bst:insert("i", T4),
    T6 = bst:insert("e", T5),
    T = bst:insert("z", T6),
    
    ?assert(bst:member("g", T) =:= true),
    ?assert(bst:member("h", T) =:= true),
    ?assert(bst:member("f", T) =:= true),
    ?assert(bst:member("i", T) =:= true),
    ?assert(bst:member("e", T) =:= true),
    ?assert(bst:member("z", T) =:= true),
    ?assert(bst:member("a", T) =:= false).
