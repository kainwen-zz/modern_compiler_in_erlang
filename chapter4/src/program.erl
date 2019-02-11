%% @author Zhenghua Lyu <kainwen@gmail.com>
%%   [http://kainwen.com/]
%%
%% @doc This module contains Chapter 1's programming exercise.

-module(program).

-include("type_defs.hrl").

-export([interp/1]).

%% Page11 Programming Exercise 2
-spec interp(stm()) -> unit().
interp(Stm) ->
    ok = env:start(),
    interp_internal(Stm).

-spec interp_internal(stm()) -> unit().
interp_internal({compound_stm, Stm1, Stm2}) ->
    interp_internal(Stm1),
    interp_internal(Stm2);
interp_internal({assign_stm, Id, Exp}) ->
    Value = eval(Exp),
    ok = env:extend_env(Id, Value),
    nil;
interp_internal({print_stm, Exps}) ->
    Values = [eval(Exp) || Exp <- Exps],
    lists:foreach(fun (I) ->
                      io:format("~p ", [I])
                  end,
                  Values),
    io:format("~n"),
    nil.

-spec eval(exp()) -> integer().
eval({id_exp, Id}) ->
    env:apply_env(Id);
eval({num_exp, Num}) -> Num;
eval({op_exp, E1, Op, E2}) ->
    V1 = eval(E1),
    V2 = eval(E2),
    case Op of
        plus -> V1 + V2;
        minus -> V1 - V2;
        times -> V1 * V2;
        'div' -> V1 div V2
    end;
eval({eseq_exp, Stm, Exp}) ->
    interp_internal(Stm),
    eval(Exp).
