-module(program4x_tests).

-include_lib("eunit/include/eunit.hrl").

interp_test() ->
    Code = "a:=6; a:=(a:=a+1, a+4)+a  ; print(a)",
    Stm = program4x_parse:scan_and_parse(Code),
    {_Table, Stdout} = program4x:interp_stm(Stm),
    ?assert(Stdout =:= [18]).
