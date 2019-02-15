-type id() :: atom().

-type binop() :: '+' | '-' | '*' | '/'.

-type stm() :: {compound_stm, stm(), stm()}
             | {assign_stm, id(), exp()}
             | {print_stm, [exp()]}.

-type exp() :: {id_exp, id()}
             | {num_exp, integer()}
             | {op_exp, exp(), binop(), exp()}
             | {eseq_exp, stm(), exp()}.

-type value() :: integer().
-type table() :: {atom(), value()}.
-type stdout() :: [value()].

-type sym() :: ';' | ':=' | ',' | '+' | '-' | '*' | '/' | '(' | ')' | ','.
-type token() :: {symbol, sym()}
               | {keyword, print}
               | {id, atom()}
               | {int, integer()}.
-type tokens() :: [token()].
