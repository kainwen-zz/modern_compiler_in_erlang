-module(program4x).

-include("type_defs.hrl").

-export([interp_stm/1]).

-spec interp_stm(stm()) -> {table(), stdout()}.
interp_stm(Stm) ->
    interp_stm(Stm, []).

-spec interp_stm(stm(), table()) -> {table(), stdout()}.
interp_stm({assign_stm, Id, Exp}, Table) ->
    {Value, NewTable, Stdout} = interp_exp(Exp, Table),
    {[{Id, Value}|NewTable], Stdout};
interp_stm({print_stm, []}, Table) ->
    {Table, []};
interp_stm({print_stm, [Exp|Exps]}, Table) ->
    {Value, T1, S1} = interp_exp(Exp, Table),
    {T2, S2} = interp_stm({print_stm, Exps}, T1),
    Stdout = [Value|S1] ++ S2,
    {T2, Stdout};
interp_stm({compound_stm, Stm1, Stm2}, Table) ->
    {T1, S1} = interp_stm(Stm1, Table),
    {T2, S2} = interp_stm(Stm2, T1),
    {T2, S1++S2}.

-spec interp_exp(exp(), table()) -> {value(), table(), stdout()}.
interp_exp({id_exp, Id}, Table) ->
    Value = get_value(Table, Id),
    {Value, Table, []};
interp_exp({num_exp, Num}, Table) ->
    {Num, Table, []};
interp_exp({op_exp, E1, Op, E2}, Table) ->
    {V1, T1, S1} = interp_exp(E1, Table),
    {V2, T2, S2} = interp_exp(E2, T1),
    Value = case Op of
                '+' ->
                    V1 + V2;
                '-' ->
                    V1 - V2;
                '*' ->
                    V1 * V2;
                '/' ->
                    V1 / V2
            end,
    {Value, T2, S1 ++ S2};
interp_exp({eseq_exp, Stm, Exp}, Table) ->
    {T1, S1} = interp_stm(Stm, Table),
    {Value, T2, S2} = interp_exp(Exp, T1),
    {Value, T2, S1 ++ S2}.

-spec get_value(table(), atom()) -> value().
get_value(Table, Id) ->
    case lists:keyfind(Id, 1, Table) of
        {Id, Value} ->
            Value;
        false ->
            erlang:error({cannot_find_key, Id})
    end.
