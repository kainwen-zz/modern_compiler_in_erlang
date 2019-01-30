%% @author Zhenghua Lyu <kainwen@gmail.com>
%%   [http://kainwen.com/]
%%
%% @doc This module contains Chapter 1's programming exercise.

-module(program).

-include("type_defs.hrl").

-export([maxargs/1, interp/1]).

%% Page11 Programming Exercise 1
-spec maxargs(stm()) -> integer().
maxargs({compound_stm, S1, S2}) ->
    max(maxargs(S1), maxargs(S2));
maxargs({assign_stm, _Id, Exp}) ->
    maxargs_for_exp(Exp);
maxargs({print_stm, Exps}) ->
    max(length(Exps),
        lists:max([maxargs_for_exp(Exp)
                   || Exp <- Exps])).

-spec maxargs_for_exp(exp()) -> integer().
maxargs_for_exp({id_exp, _}) -> 0;
maxargs_for_exp({num_exp, _}) -> 0;
maxargs_for_exp({op_exp, E1, _Op, E2}) ->
    max(maxargs_for_exp(E1),
        maxargs_for_exp(E2));
maxargs_for_exp({eseq_exp, Stm, Exp}) ->
    max(maxargs(Stm),
        maxargs_for_exp(Exp)).

%% Page11 Programming Exercise 2
-spec interp(stm()) -> unit().
interp(Stm) ->
    _Env = interp(Stm, env:init()),
    nil.

-spec interp(stm(), env:env()) -> env:env().
interp({compound_stm, Stm1, Stm2}, Env) ->
    Env1 = interp(Stm1, Env),
    Env2 = interp(Stm2, Env1),
    Env2;
interp({assign_stm, Id, Exp}, Env) ->
    Value = eval(Exp, Env),
    env:extend_env(Env, Id, Value);
interp({print_stm, Exps}, Env) ->
    Values = [eval(Exp, Env) || Exp <- Exps],
    lists:foreach(fun (I) ->
                      io:format("~p ", [I])
                  end,
                  Values),
    io:format("~n"),
    Env.

-spec eval(exp(), env:env()) -> integer().
eval({id_exp, Id}, Env) ->
    env:apply_env(Env, Id);
eval({num_exp, Num}, _Env) -> Num;
eval({op_exp, E1, Op, E2}, Env) ->
    V1 = eval(E1, Env),
    V2 = eval(E2, Env),
    case Op of
        plus -> V1 + V2;
        minus -> V1 - V2;
        times -> V1 * V2;
        'div' -> V1 div V2
    end;
eval({eseq_exp, Stm, Exp}, Env) ->
    Env1 = interp(Stm, Env),
    eval(Exp, Env1).
