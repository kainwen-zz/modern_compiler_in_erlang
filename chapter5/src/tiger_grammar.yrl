Nonterminals program
             exp while_exp lvalue_exp nil_exp sequence_exp no_value_exp integer_exp
             string_exp negation_exp function_call_exp binary_op_exp
             block_exp record_create_exp array_create_exp assignment_exp
             if_then_else_exp if_then_exp for_loop_exp break_exp let_exp
             more_than_two_exps arglist field_exp_pairs binary_op
             lvalue_compound declarations letbody declaration type_declaration
             var_declaration function_declaration
             ty type_fields.

Terminals identifier '.' '[' ']' while 'do' nil ';' '(' ')' integer_const string_const
          '-' ',' '*' '+' '/' '|' '&' '<>' '=' '<' '>' '>=' '<=' '{' '}' 'of' 'type'
          ':=' 'if' 'then' 'else' 'for' 'to' break 'in' 'array' 'let' 'end' ':'
          'var' 'function'.

Rootsymbol program.

Left 100 '&'.
Left 100 '|'.
Nonassoc 200 '<'.
Nonassoc 200 '>'.
Nonassoc 200 '<>'.
Nonassoc 200 '<='.
Nonassoc 200 '>='.
Nonassoc 200 '='.
Left 300 '+'.
Left 300 '-'.
Left 400 '*'.
Left 400 '/'.
Unary 500 negation_exp.

program ->
    exp :
    '$1'.

exp ->
    lvalue_exp :
    '$1'.

exp ->
    while_exp :
    '$1'.

exp ->
    nil_exp :
    '$1'.

exp ->
    sequence_exp :
    '$1'.

exp ->
    no_value_exp :
    '$1'.

exp ->
    integer_exp :
    '$1'.

exp ->
    string_exp :
    '$1'.

exp ->
    negation_exp :
    '$1'.

exp ->
    function_call_exp :
    '$1'.

exp ->
    binary_op_exp :
    '$1'.

exp ->
    block_exp :
    '$1'.

exp ->
    record_create_exp :
    '$1'.

exp ->
    array_create_exp :
    '$1'.

exp ->
    assignment_exp :
    '$1'.

exp ->
    if_then_else_exp :
    '$1'.

exp ->
    if_then_exp :
    '$1'.

exp ->
    for_loop_exp :
    '$1'.

exp ->
    break_exp :
    '$1'.

exp ->
    let_exp :
    '$1'.

lvalue_exp ->
    identifier :
    '$1'.

lvalue_exp ->
    lvalue_compound :
    '$1'.

lvalue_compound ->
    identifier '.' identifier :
    {'.', TokenLine} = '$2', 
    {record_field, TokenLine, '$1', '$3'}.

lvalue_compound ->
    lvalue_compound '.' identifier :
    {'.', TokenLine} = '$2', 
    {record_field, TokenLine, '$1', '$3'}.

lvalue_compound ->
    identifier '[' exp ']' :
    {'[', TokenLine} = '$2',
    {array_ref, TokenLine, '$1', '$3'}.

lvalue_compound ->
    lvalue_compound '[' exp ']' :
    {'[', TokenLine} = '$2',
    {array_ref, TokenLine, '$1', '$3'}.

while_exp ->
    while exp 'do' exp :
    {while, TokenLine} = '$1',
    {while_exp, TokenLine, '$2', '$4'}.

nil_exp ->
    nil :
    {nil, TokenLine} = '$1',
    {nil_exp, TokenLine}.

sequence_exp ->
    '(' more_than_two_exps ')' :
    {'(', TokenLine} = '$1',
    {sequence_exp, TokenLine, '$2'}.

more_than_two_exps ->
    exp ';' exp :
    ['$1', '$3'].

more_than_two_exps ->
    exp ';' more_than_two_exps :
    ['$1'|'$3'].

no_value_exp ->
    '(' ')' :
    {'(', TokenLine} = '$1',
    {no_value_exp, TokenLine}.

integer_exp ->
    integer_const :
    '$1'.

string_exp ->
    string_const :
    '$1'.

negation_exp ->
    '-' exp :
    {'-', TokenLine} = '$1',
    {negation_exp, TokenLine, '$2'}.

function_call_exp ->
    identifier '(' ')' :
    {identifier, TokenLine, _} = '$1',
    {function_call_exp, TokenLine, '$1', []}.

function_call_exp ->
    identifier '(' arglist ')' :
    {identifier, TokenLine, _} = '$1',
    {function_call_exp, TokenLine, '$1', '$3'}.

arglist ->
    exp :
    ['$1'].

arglist ->
    exp ',' arglist:
    ['$1'|'$3'].

binary_op_exp ->
    exp binary_op exp :
    {Op, TokenLine} = '$2',
    Exp_type_info = [
                     {'+', arith_exp},
                     {'-', arith_exp},
                     {'*', arith_exp},
                     {'/', arith_exp},
                     {'<>', compare_exp},
                     {'<', compare_exp},
                     {'>', compare_exp},
                     {'>=', compare_exp},
                     {'=<', compare_exp},
                     {'=', compare_exp},
                     {'&', bool_exp},
                     {'|', bool_exp}
                    ],
    {Op, Exp_type} = lists:keyfind(Op, 1, Exp_type_info),
    {Exp_type, TokenLine, Op, '$1', '$3'}.

binary_op ->
    '+' :
    '$1'.

binary_op ->
    '-' :
    '$1'.

binary_op ->
    '*' :
    '$1'.

binary_op ->
    '/' :
    '$1'.

binary_op ->
    '<' :
    '$1'.
    
binary_op ->
    '>' :
    '$1'.

binary_op ->
    '<>' :
    '$1'.

binary_op ->
    '=' :
    '$1'.

binary_op ->
    '<=' :
    '$1'.

binary_op ->
    '>=' :
    '$1'.

binary_op ->
    '&' :
    '$1'.

binary_op ->
    '|' :
    '$1'.

block_exp ->
    '(' exp ')' :
    {'(', TokenLine} = '$1',
    {block_exp, TokenLine, '$2'}.

record_create_exp ->
    identifier '{' '}' :
    {'{', TokenLine} = '$2',
    {record_create_exp, TokenLine, '$1', []}.

record_create_exp ->
    identifier '{' field_exp_pairs '}' :
    {'{', TokenLine} = '$2',
    {record_create_exp, TokenLine, '$1', '$3'}.

field_exp_pairs ->
    identifier '=' exp :
    [{'$1', '$3'}].

field_exp_pairs ->
    identifier '=' exp ',' field_exp_pairs:
    [{'$1', '$3'}|'$5'].

array_create_exp ->
    identifier '[' exp ']' 'of' exp :
    {'[', TokenLine} = '$2',
    {array_create_exp, TokenLine, '$1', '$3', '$6'}.

assignment_exp ->
    lvalue_exp ':=' exp :
    {':=', TokenLine} = '$2',
    {assignment_exp, TokenLine, '$1', '$3'}.

if_then_else_exp ->
    'if' exp 'then' exp 'else' exp :
    {'if', TokenLine} = '$1',
    {if_then_else_exp, TokenLine, '$2', '$4', '$6'}.

if_then_exp ->
    'if' exp 'then' exp :
    {'if', TokenLine} = '$1',
    {if_then_exp, TokenLine, '$2', '$4'}.

for_loop_exp ->
    'for' identifier ':=' exp 'to' exp 'do' exp :
    {'for', TokenLine} = '$1',
    {for_loop_exp, TokenLine, '$2', '$4', '$6', '$8'}.

break_exp ->
    break :
    {break, TokenLine} = '$1',
    {break_exp, TokenLine}.

let_exp ->
    'let' declarations 'in' letbody 'end' :
    {'let', TokenLine} = '$1',
    {let_exp, TokenLine, '$2', '$4'}.
    

declarations ->
    '$empty' :
    [].

declarations ->
    declaration declarations :
    ['$1'|'$2'].

declaration ->
    type_declaration :
    '$1'.

declaration ->
    var_declaration :
    '$1'.

declaration ->
    function_declaration :
    '$1'.

type_declaration ->
    'type' identifier '=' ty :
    {'type', TokenLine} = '$1',
    {type_declaration, TokenLine, '$2', '$4'}.

ty ->
    identifier :
    '$1'.

ty ->
    '{' type_fields '}' :
    {'{', TokenLine} = '$1',
    {record_type, TokenLine, '$2'}.

ty ->
    'array' 'of' identifier :
    {'array', TokenLine} = '$1',
    {array_type, TokenLine, '$3'}.

type_fields ->
    '$empty' :
    [].

type_fields ->
    identifier ':' identifier :
    [{'$1', '$3'}].

type_fields ->
    identifier ':' identifier ',' type_fields :
    [{'$1', '$3'}|'$5'].

var_declaration ->
    'var' identifier ':=' exp :
    {'var', TokenLine} = '$1',
    {var_declaration, TokenLine, '$2', void, '$4'}.

var_declaration ->
    'var' identifier ':' identifier ':=' exp :
    {'var', TokenLine} = '$1',
    {var_declaration, TokenLine, '$2', '$4', '$6'}.

function_declaration ->
    'function' identifier '(' type_fields ')' '=' exp :
    {'function', TokenLine} = '$1',
    {function_declaration, TokenLine, '$2', void, '$4', '$7'}.

function_declaration ->
    'function' identifier '(' type_fields ')' ':' identifier '=' exp :
    {'function', TokenLine} = '$1',
    {function_declaration, TokenLine, '$2', '$7', '$4', '$9'}.

letbody ->
    '$empty' :
    [].

letbody ->
    exp :
    ['$1'].

letbody ->
    exp ';' letbody :
    ['$1'|'$3'].
