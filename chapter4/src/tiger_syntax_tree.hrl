-type program() :: exp().

-type exp() :: lvalue_exp()
             | while_exp()
             | nil_exp()
             | sequence_exp()
             | no_value_exp()
             | integer_exp()
             | string_exp()
             | negation_exp()
             | function_call_exp()
             | arith_exp()
             | bool_exp()
             | compare_exp()
             | block_exp()
             | record_create_exp()
             | array_create_exp()
             | assignment_exp()
             | if_then_else_exp()
             | if_then_exp()
             | for_loop_exp()
             | break_exp()
             | let_exp().

-type tiger_id() :: {identifier, TokenLine::integer(), atom()}. 

-type lvalue_exp() :: tiger_id()
                | {record_field, TokenLine::integer(), lvalue_exp(), tiger_id()}
                | {array_ref, TokenLine::integer(), lvalue_exp(), exp()}.

-type while_exp() :: {while_exp, TokenLine::integer(), exp(), exp()}.

-type nil_exp() :: {nil_exp, TokenLine::integer()}.

-type sequence_exp() :: {sequence_exp, TokenLine::integer(), [exp()]}.

-type no_value_exp() :: {no_value_exp, TokenLine::integer()}.

-type integer_exp() :: {integer_const, TokenLine::integer(), integer()}.

-type string_exp() :: {string_const, TokenLine::integer(), string()}.

-type negation_exp() :: {negation_exp, TokenLine::integer(), exp()}.

-type function_call_exp() :: {function_call_exp, TokenLine::integer(),
                              tiger_id(), [exp()]}.

-type arith_op() :: '+' | '-' | '*' | '/'.
-type arith_exp() :: {arith_exp, TokenLine::integer(), arith_op(), exp(), exp()}.

-type bool_op() :: '&' | '|'.
-type bool_exp() :: {bool_exp, TokenLine::integer(), bool_op(), exp(), exp()}.

-type compare_op() :: '=' | '<>' | '<' | '>' | '<=' | '>='.
-type compare_exp() :: {compare_exp, TokenLine::integer(), compare_op(), exp(), exp()}.

-type block_exp() :: {block_exp(), TokenLine::integer(), exp()}.

-type record_create_exp() :: {record_create_exp, 
                              TokenLine::integer(), tiger_id(), [{tiger_id(), exp()}]}.

-type type_id() :: tiger_id().
-type array_create_exp() :: {array_create_exp, TokenLine::integer(),
                             type_id(), ArrayLength::exp(), InitValue::exp()}.

-type assignment_exp() :: {assignment_exp, TokenLine::integer(),
                           lvalue_exp(), exp()}.

-type if_then_else_exp() :: {if_then_else_exp, TokenLine::integer(),
                             exp(), exp(), exp()}.

-type if_then_exp() :: {if_then_exp, TokenLine::integer(),
                        exp(), exp()}.

-type for_loop_exp() :: {for_loop_exp, TokenLine::integer(),
                         LoopVar::tiger_id(),
                         StartValue::exp(),
                         EndValue::exp(),
                         Body::exp()}.

-type break_exp() :: {break_exp, TokenLine::integer()}.

-type type_field() :: {identifier(), identifier()}.
-type ty() :: tiger_id()
            | {record_type, TokenLine::integer(), [type_field()]}
            | {array_type, TokenLine::integer(), tiger_id()}.
-type type_declaration() :: {type_declaration, TokenLine::integer(), tiger_id(), ty()}.
-type var_type() :: nil | tiger_id().
-type var_declaration() :: {var_declaration, TokenLine::integer(), tiger_id(),
                            var_type(), exp()}.
-type function_declaration() :: {function_declaration, TokenLine::integer(),
                                 tiger_id(), var_type(), [type_field()], exp()}.
-type declaration() :: type_declaration()
                     | var_declaration()
                     | function_declaration().
-type declarations() :: [declaration()].
-type letbody() :: [exp()].
-type let_exp() :: {let_exp, TokenLine::integer(),
                    declarations(), letbody()}.
