-module(tiger_tests).

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
                                                     void,
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
                                                     void,
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
                                                     void,
                                                     [{{identifier,3,i},
                                                       {identifier,3,int}}],
                                                     {function_call_exp,3,
                                                      {identifier,3,f},[]}}],
                                                   [{function_call_exp,5,
                                                     {identifier,5,f},[]}]}).

%% typer test
const_typer_test() ->
    Code1 = "1",
    Exp1 = tiger_parse:scan_and_parse(Code1),
    ?assert(tiger_typer:typer(Exp1) =:= int),
    Code2 = "\"abc\"",
    Exp2 = tiger_parse:scan_and_parse(Code2),
    ?assert(tiger_typer:typer(Exp2) =:= string).

var_binding_typer_test() ->
    Code = "let type tree = {key: int, children: treelist}
                type treelist = {hd: tree, tl: treelist}
            in
              let var t := tree {key=1, children=nil}
              in
                t.children
              end
            end",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= treelist).

array_typer_test() ->
    Code = "let var a := int [3] of 1
            in
                 a[1]
            end",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= int).

sequence_typer_test() ->
    Code = "(1; 2; let var a := int [3] of 1 in a[1] end)",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= int).

negation_typer_test() ->
    Code = "let var a := int [3] of 1
            in
                 -a[1]
            end",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= int).

function_call_typer_test() ->
    Code1 = "let function f (n : int) : int =
              n
            in
              f(3)
            end",
    Exp1 = tiger_parse:scan_and_parse(Code1),
    ?assert(tiger_typer:typer(Exp1) =:= int),
    Code2 = "let function fib (n : int) : int =
                 fib (n)
             in
                 fib (2)
             end",
    Exp2 = tiger_parse:scan_and_parse(Code2),
    ?assert(tiger_typer:typer(Exp2) =:= int),
    Code3 = "let type list = {hd: int, tl: list}
             in
                let function f (l : list) : int =
                    3
                in
                    (f (nil);
                     f (list {hd=1, tl=nil}))
                end
             end",
    Exp3 = tiger_parse:scan_and_parse(Code3),
    ?assert(tiger_typer:typer(Exp3) =:= int).
    
arith_exp_typer_test() ->    
    Code = "1 + 2",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= int).

bool_exp_typer_test() ->
    Code = "1 | 2",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= int).

com_exp_typer_test() ->
    Code = "1 > 2",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= int).

block_exp_typer_test() ->
    Code = "(1+2)",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= int).


assign_exp_typer_test() ->
    Code = "let var a := 3
            in
                a := 4
            end",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= noval).

if_then_else_exp_typer_test() ->
    Code = "let type person = {name : string, age : int}
            in
                let var ps := person [10] of person {name = \"test\", age=10}
                in
                   if ps[1].age > 10
                   then ps[1].name
                   else \"haha\"
                end
            end",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= string).

if_then_exp_typer_test() ->
    Code = "let type person = {name : string, age : int}
            in
                let var ps := person [10] of person {name = \"test\", age=10}
                in
                   if ps[1].age > 10
                   then ps[1].name
                end
            end",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= noval).

for_loop_exp_typer_test() ->
    Code = "for i := 1 to 3 do
              if i > 1
              then
                5
              else
                6",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= int).

while_exp_typer_test() ->
    Code = "let var i := 1
            in
               while i < 10 do
                 i := i + 1
            end",
    Exp = tiger_parse:scan_and_parse(Code),
    ?assert(tiger_typer:typer(Exp) =:= noval).
