-module(chapter1_program_tests).

-include_lib("eunit/include/eunit.hrl").

maxargs_test() ->
    Prog = {compound_stm,
            {assign_stm,
             "b",
             {eseq_exp,
              {print_stm, [{id_exp, "a"},
                           {op_exp,
                            {id_exp, "a"},
                            minus,
                            {num_exp, 1}}]},
              {op_exp,
               {num_exp, 10},
               times,
               {id_exp, "a"}}}},
            {print_stm, [{id_exp, "b"}]}},
    ?assert(chapter1_program:maxargs(Prog) =:= 2).

