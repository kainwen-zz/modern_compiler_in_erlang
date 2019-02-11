-module(tiger_parse_test).

-include_lib("eunit/include/eunit.hrl").

lvalue_test() ->
    Code1 = "x",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {identifier, 1, x}),
    Code2 = "x.y",
    ?assert(tiger_parse:scan_and_parse(Code2) =:= {record_field, 1,
                                                   {identifier, 1, x},
                                                   {identifier, 1, y}}),
    Code3 = "x[a]",
    ?assert(tiger_parse:scan_and_parse(Code3) =:= {array_ref, 1,
                                                   {identifier, 1, x},
                                                   {identifier, 1, a}}).

while_exp_test() ->
    Code1 = "while x.b do x.a",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {while_exp, 1,
                                                   {record_field, 1,
                                                    {identifier, 1, x},
                                                    {identifier, 1, b}},
                                                   {record_field, 1,
                                                    {identifier, 1, x},
                                                    {identifier, 1, a}}}).

nil_exp_test() ->
    Code1 = "nil",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {nil_exp, 1}).

sequence_exp_test() ->
    Code1 = "(nil; a; a.x)",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {sequence_exp, 1,
                                                   [{nil_exp, 1},
                                                    {identifier, 1, a},
                                                    {record_field, 1,
                                                     {identifier, 1, a},
                                                     {identifier, 1, x}}]}).

no_value_exp_test() ->
    Code1 = "()",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {no_value_exp, 1}).

integer_exp_test() ->
    Code1 = "2",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {integer_const, 1, 2}).

string_exp_test() ->
    Code1 = "\"abc\\065\"",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {string_const, 1,
                                                   "abcA"}).

negation_exp_test() ->
    Code1 = "-a",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {negation_exp, 1,
                                                   {identifier, 1, a}}).

function_call_exp_test() ->
    Code1 = "f(x, y.a, -z, 123, a[1])",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {function_call_exp, 1,
                                                   {identifier, 1, f},
                                                   [{identifier, 1, x},
                                                    {record_field, 1,
                                                     {identifier, 1, y},
                                                     {identifier, 1, a}},
                                                    {negation_exp, 1,
                                                     {identifier, 1, z}},
                                                    {integer_const, 1, 123},
                                                    {array_ref, 1,
                                                     {identifier, 1, a},
                                                     {integer_const, 1, 1}}]}).

arith_exp_test() ->
    Code1 = "-1 + a.x + f(2) * b.y / -3",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {arith_exp,1,'+',
                                                   {negation_exp,1,
                                                    {integer_const,1,1}},
                                                   {arith_exp,1,'+',
                                                    {record_field,1,
                                                     {identifier,1,a},
                                                     {identifier,1,x}},
                                                    {arith_exp,1,'*',
                                                     {function_call_exp,1,
                                                      {identifier,1,f},
                                                      [{integer_const,1,2}]},
                                                     {arith_exp,1,'/',
                                                      {record_field,1,
                                                       {identifier,1,b},
                                                       {identifier,1,y}},
                                                      {negation_exp,1,
                                                       {integer_const,1,3}}}}}}),
    Code2 = "1+2*(3+4)",
    ?assert(tiger_parse:scan_and_parse(Code2) =:= {arith_exp,1,'+',
                                                   {integer_const,1,1},
                                                   {arith_exp,1,'*',
                                                    {integer_const,1,2},
                                                    {block_exp,1,
                                                     {arith_exp,1,'+',
                                                      {integer_const,1,3},
                                                      {integer_const,1,4}}}}}).

compare_exp_test() ->
    Code1 = "1 <> 2",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {compare_exp, 1, '<>',
                                                   {integer_const, 1, 1},
                                                   {integer_const, 1, 2}}),
    Code2 = "\"abc\" > \"cde\"",
    ?assert(tiger_parse:scan_and_parse(Code2) =:= {compare_exp, 1, '>',
                                                   {string_const, 1, "abc"},
                                                   {string_const, 1, "cde"}}).

bool_exp_test() ->
    Code1 = "1 | 2",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {bool_exp, 1, '|',
                                                   {integer_const, 1, 1},
                                                   {integer_const, 1, 2}}).

record_create_exp_test() ->
    Code1 = "person{name=\"wn\", age=17}",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {record_create_exp, 1,
                                                   {identifier, 1, person},
                                                   [{{identifier, 1, name},
                                                     {string_const, 1, "wn"}},
                                                    {{identifier, 1, age},
                                                     {integer_const, 1, 17}}]}).

array_create_exp_test() ->
    Code1 = "int[4] of 2",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {array_create_exp, 1,
                                                   {identifier, 1, int},
                                                   {integer_const, 1, 4},
                                                   {integer_const, 1, 2}}),
    Code2 = "f[4] of 3",
    ?assert(tiger_parse:scan_and_parse(Code2) =:= {array_create_exp, 1,
                                                   {identifier, 1, f},
                                                   {integer_const, 1, 4},
                                                   {integer_const, 1, 3}}).

assignment_exp_test() ->
    Code1 = "a.x := y",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {assignment_exp, 1,
                                                   {record_field, 1,
                                                    {identifier, 1, a},
                                                    {identifier, 1, x}},
                                                   {identifier, 1, y}}).

if_then_else_exp_test() ->
    Code1 = "if a[3] > 0 then a := a + 1 else b.x[3]",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {if_then_else_exp, 1,
                                                   {compare_exp, 1, '>',
                                                    {array_ref, 1,
                                                     {identifier, 1, a},
                                                     {integer_const, 1, 3}},
                                                    {integer_const, 1, 0}},
                                                   {assignment_exp, 1,
                                                    {identifier, 1, a},
                                                    {arith_exp, 1, '+',
                                                     {identifier, 1, a},
                                                     {integer_const, 1, 1}}},
                                                   {array_ref, 1,
                                                    {record_field, 1,
                                                     {identifier, 1, b},
                                                     {identifier, 1, x}},
                                                    {integer_const, 1, 3}}}).

if_then_exp_test() ->
    Code1 = "if a[3] > 0 then a := a + 1",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {if_then_exp, 1,
                                                   {compare_exp, 1, '>',
                                                    {array_ref, 1,
                                                     {identifier, 1, a},
                                                     {integer_const, 1, 3}},
                                                    {integer_const, 1, 0}},
                                                   {assignment_exp, 1,
                                                    {identifier, 1, a},
                                                    {arith_exp, 1, '+',
                                                     {identifier, 1, a},
                                                     {integer_const, 1, 1}}}}).

for_loop_exp_test() ->
    Code1 = "for i := 1 to a+3 do y:=-y",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {for_loop_exp, 1,
                                                   {identifier, 1, i},
                                                   {integer_const, 1, 1},
                                                   {arith_exp, 1, '+',
                                                    {identifier, 1, a},
                                                    {integer_const, 1, 3}},
                                                   {assignment_exp, 1,
                                                    {identifier, 1, y},
                                                    {negation_exp, 1,
                                                     {identifier, 1, y}}}}).
                                                    

break_exp_test() ->
    Code1 = "break",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {break_exp, 1}).

let_exp_test() ->
    Code1 = "let type a = {name:string, age:int} in a{name=\"wn\", age=7} end",
    ?assert(tiger_parse:scan_and_parse(Code1) =:= {let_exp, 1,
                                                   [{type_declaration, 1,
                                                     {identifier, 1, a},
                                                     {record_type, 1,
                                                      [{{identifier, 1, name},
                                                        {identifier, 1, string}},
                                                       {{identifier, 1, age},
                                                        {identifier, 1, int}}]}}],
                                                   [{record_create_exp, 1,
                                                     {identifier, 1, a},
                                                     [{{identifier, 1, name},
                                                       {string_const, 1, "wn"}},
                                                      {{identifier, 1, age},
                                                       {integer_const, 1, 7}}]}]}),
    Code2 = "let type a = array of int in a[4] of 3 ; 5 + 3 end",
    ?assert(tiger_parse:scan_and_parse(Code2) =:= {let_exp, 1,
                                                   [{type_declaration, 1,
                                                     {identifier, 1, a},
                                                     {array_type, 1,
                                                      {identifier, 1, int}}}],
                                                   [{array_create_exp, 1,
                                                     {identifier, 1, a},
                                                     {integer_const, 1, 4},
                                                     {integer_const, 1, 3}},
                                                    {arith_exp, 1, '+',
                                                     {integer_const, 1, 5},
                                                     {integer_const, 1, 3}}]}),
    Code3 = "let var a := b[3] in a := 1 end",
    ?assert(tiger_parse:scan_and_parse(Code3) =:= {let_exp, 1,
                                                   [{var_declaration, 1,
                                                     {identifier, 1, a},
                                                     nil,
                                                     {array_ref, 1,
                                                      {identifier, 1, b},
                                                      {integer_const, 1, 3}}}],
                                                   [{assignment_exp, 1,
                                                     {identifier, 1, a},
                                                     {integer_const, 1, 1}}]}),
    Code4 = "let function f(a: int, b: int) : int = (b;a) in f(1,2) end",
    ?assert(tiger_parse:scan_and_parse(Code4) =:= {let_exp, 1,
                                                   [{function_declaration, 1,
                                                     {identifier, 1, f},
                                                     {identifier, 1, int},
                                                     [{{identifier, 1, a},
                                                       {identifier, 1, int}},
                                                      {{identifier, 1, b},
                                                       {identifier, 1, int}}],
                                                     {sequence_exp, 1,
                                                      [{identifier, 1, b},
                                                       {identifier, 1, a}]}}],
                                                   [{function_call_exp, 1,
                                                     {identifier, 1, f},
                                                     [{integer_const, 1, 1},
                                                      {integer_const, 1, 2}]}]}),
    Code5 = "let var a := 5
               function f() : int = g(a)
               function g(i: int) = f()
             in
               f()
             end",
    ?assert(tiger_parse:scan_and_parse(Code5) =:= {let_exp,1,
                                                   [{var_declaration,1,
                                                     {identifier,1,a},
                                                     nil,
                                                     {integer_const,1,5}},
                                                    {function_declaration,2,
                                                     {identifier,2,f},
                                                     {identifier,2,int},
                                                     [],
                                                     {function_call_exp,2,
                                                      {identifier,2,g},
                                                      [{identifier,2,a}]}},
                                                    {function_declaration,3,
                                                     {identifier,3,g},
                                                     nil,
                                                     [{{identifier,3,i},
                                                       {identifier,3,int}}],
                                                     {function_call_exp,3,
                                                      {identifier,3,f},[]}}],
                                                   [{function_call_exp,5,
                                                     {identifier,5,f},[]}]}).
