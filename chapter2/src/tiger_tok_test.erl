-module(tiger_tok_test).

-include_lib("eunit/include/eunit.hrl").

check([]) -> ok;
check([{Code, Expected}|Rems]) ->
    {ok, [Tok], _} = tiger_tok:string(Code),
    ?assert(Tok =:= Expected),
    check(Rems).

string_tok_test() ->
    check([
           {"\"123 456 789\"", {string, "123 456 789"}},
           {"\"\\n\\t\"", {string, "\n\t"}},
           {"\"\\065\"", {string, "A"}},
           {"\"\\\\\"", {string, "\\"}}
          ]).

int_tok_test() ->
    check([
           {"123", {integer, 123}}
          ]).
