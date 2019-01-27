-type id() :: string().

-type binop() :: plus | minus | times | 'div'.

-type stm() :: {compound_stm, stm(), stm()}
             | {assign_stm, id(), exp()}
             | {print_stm, [exp()]}.

-type exp() :: {id_exp, id()}
             | {num_exp, integer()}
             | {op_exp, exp(), binop(), exp()}
             | {eseq_exp, stm(), exp()}.

-type unit() :: nil.
