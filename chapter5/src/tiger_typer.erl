-module(tiger_typer).

-include("tiger_syntax_tree.hrl").

-export([typer/1]).

-define(PREFIX, "&&[]&&").

typer(Exp) ->
    TypeEnv = env:put(env:new(),
                      [{int, int}, {string, string}, {nil, nil}, {noval, noval}]),
    VarEnv = env:new(),
    typer(Exp, TypeEnv, VarEnv).

typer(Exp={let_exp, _, _Decs, _Body}, TypeEnv, VarEnv) ->
    handle_typer_letexp(Exp, TypeEnv, VarEnv);
typer({integer_const, _, _Int}, _, _) ->
    int;
typer({string_const, _, _Str}, _, _) ->
    string;
typer({identifier, _, Var}, _TypeEnv, VarEnv) ->
    env:lookup(VarEnv, Var);
typer(Exp={record_field, _, _, _}, TypeEnv, VarEnv) ->
    handle_typer_record_field(Exp, TypeEnv, VarEnv);
typer(Exp={record_create_exp, _, _, _}, TypeEnv, VarEnv) ->
    handle_typer_record_create_exp(Exp, TypeEnv, VarEnv);
typer({nil_exp, _}, _, _) ->
    nil;
typer(Exp={array_ref, _, _, _}, TypeEnv, VarEnv) ->
    handle_array_ref_exp(Exp, TypeEnv, VarEnv);
typer(Exp={array_create_exp, _, _, _, _}, TypeEnv, VarEnv) ->
    handle_array_create_exp(Exp, TypeEnv, VarEnv);
typer(Exp={sequence_exp, _, _}, TypeEnv, VarEnv) ->
    handle_sequence_exp(Exp, TypeEnv, VarEnv);
typer({no_value_exp, _}, _, _) -> noval;
typer(Exp={negation_exp, _, _}, TypeEnv, VarEnv) ->
    handle_negation_exp(Exp, TypeEnv, VarEnv);
typer(Exp={function_call_exp, _, _, _}, TypeEnv, VarEnv) ->
    handle_function_call_exp(Exp, TypeEnv, VarEnv);
typer(Exp={arith_exp, _, _, _, _}, TypeEnv, VarEnv) ->
    handle_arith_exp(Exp, TypeEnv, VarEnv);
typer(Exp={bool_exp, _, _, _, _}, TypeEnv, VarEnv) ->
    handle_bool_exp(Exp, TypeEnv, VarEnv);
typer(Exp={compare_exp, _, _, _, _}, TypeEnv, VarEnv) ->
    handle_cmp_exp(Exp, TypeEnv, VarEnv);
typer({block_exp, _, Exp}, TypeEnv, VarEnv) ->
    typer(Exp, TypeEnv, VarEnv);
typer(Exp={assignment_exp, _, _, _}, TypeEnv, VarEnv) ->
    handle_assign_exp(Exp, TypeEnv, VarEnv);
typer(Exp={if_then_else_exp, _, _, _, _}, TypeEnv, VarEnv) ->
    handle_if_then_else_exp(Exp, TypeEnv, VarEnv);
typer(Exp={if_then_exp, _, _, _}, TypeEnv, VarEnv) ->
    handle_if_then_exp(Exp, TypeEnv, VarEnv);
typer(Exp={for_loop_exp, _, _, _, _, _}, TypeEnv, VarEnv) ->
    handle_for_loop_exp(Exp, TypeEnv, VarEnv);
typer(Exp={while_exp, _, _, _}, TypeEnv, VarEnv) ->
    handle_while_exp(Exp, TypeEnv, VarEnv);
typer({break_exp, _}, _, _) ->
    noval.

%% Internal functions
handle_while_exp({while_exp, _, Cond, Body}, TypeEnv, VarEnv) ->
    ok = typer_assert(typer(Cond, TypeEnv, VarEnv) =:= int,
                      "while exp condition should be bool"),
    typer(Body, TypeEnv, VarEnv).

handle_for_loop_exp({for_loop_exp, _,
                     {identifier, _, LoopVar},
                     StartValue,
                     EndValue,
                     Body}, TypeEnv, VarEnv) ->
    ok = typer_assert(typer(StartValue, TypeEnv, VarEnv) =:= int,
                      "for exp start value should be int"),
    ok = typer_assert(typer(EndValue, TypeEnv, VarEnv) =:= int,
                      "for exp start value should be int"),
    NewVarEnv = env:put(VarEnv, [{LoopVar, int}]),
    typer(Body, TypeEnv, NewVarEnv).

handle_if_then_exp({if_then_exp, _, Cond, Then}, TypeEnv, VarEnv) ->
    ok = typer_assert(typer(Cond, TypeEnv, VarEnv) =:= int,
                      "if's condition should eval to bool"),
    typer(Then, TypeEnv, VarEnv),
    noval.

handle_if_then_else_exp({if_then_else_exp, _, Cond, Then, Else}, TypeEnv, VarEnv) ->
    ok = typer_assert(typer(Cond, TypeEnv, VarEnv) =:= int,
                      "if's condition should eval to bool"),
    TypeThen = typer(Then, TypeEnv, VarEnv),
    TypeElse = typer(Else, TypeEnv, VarEnv),
    ok = typer_assert(TypeThen =:= TypeElse,
                      "then and else should type match"),
    TypeThen.

handle_assign_exp({assignment_exp, _, L, Exp}, TypeEnv, VarEnv) ->
    ok = typer_assert(typer(L, TypeEnv, VarEnv) =:=
                          typer(Exp, TypeEnv, VarEnv),
                      "assignment should keep type consisitent"),
    noval.

handle_cmp_exp({compare_exp, _, _, E1, E2}, TypeEnv, VarEnv) ->
    ok = typer_assert(typer(E1, TypeEnv, VarEnv) =:= int,
                      "arith_exp's left should be int type"),
    ok = typer_assert(typer(E2, TypeEnv, VarEnv) =:= int,
                      "arith_exp's right should be int type"),
    int.

handle_bool_exp({bool_exp, _, _, E1, E2}, TypeEnv, VarEnv) ->
    ok = typer_assert(typer(E1, TypeEnv, VarEnv) =:= int,
                      "arith_exp's left should be int type"),
    ok = typer_assert(typer(E2, TypeEnv, VarEnv) =:= int,
                      "arith_exp's right should be int type"),
    int.

handle_arith_exp({arith_exp, _, _, E1, E2}, TypeEnv, VarEnv) ->
    ok = typer_assert(typer(E1, TypeEnv, VarEnv) =:= int,
                      "arith_exp's left should be int type"),
    ok = typer_assert(typer(E2, TypeEnv, VarEnv) =:= int,
                      "arith_exp's right should be int type"),
    int.

handle_function_call_exp({function_call_exp, _,
                          {identifier, _, FuncName}, Args},
                         TypeEnv, VarEnv) ->
    FuncTypeId = gen_func_type_name(FuncName),
    {type, {function, ParaTypes, RtypeId}} = lookup_type(TypeEnv, FuncTypeId),
    ArgTypes = lists:map(fun (Arg) ->
                                 typer(Arg, TypeEnv, VarEnv)
                         end, Args),
    ok = lists:foreach(fun ({ParaType, ArgType}) ->
                               ok = verify_para_arg(ParaType, ArgType, TypeEnv)
                       end, lists:zip(ParaTypes, ArgTypes)),
    RtypeId.
    
handle_negation_exp({negation_exp, _, Exp}, TypeEnv, VarEnv) ->
    ok = typer_assert(typer(Exp, TypeEnv, VarEnv) =:= int,
                      "negation can only precede int value"),
    int.

handle_sequence_exp({sequence_exp, _, Exps}, TypeEnv, VarEnv) ->
    lists:last(lists:map(fun (Exp) ->
                                 typer(Exp, TypeEnv, VarEnv)
                         end,
                         Exps)).

handle_array_ref_exp({array_ref, _, LExp, Exp}, TypeEnv, VarEnv) ->
    ArrayType = typer(LExp, TypeEnv, VarEnv),
    {type, {array, TypeId}} = lookup_type(TypeEnv, ArrayType),
    ok = typer_assert(typer(Exp, TypeEnv, VarEnv) =:= int,
                      "array ref index should be integer"),
    TypeId.

handle_array_create_exp({array_create_exp, _,
                         {identifier, _, TypeId}, LenExp, InitValueExp},
                        TypeEnv, VarEnv) ->
    ok = typer_assert(env:contain(TypeEnv, TypeId) =:= true,
                      "array type id does not exist"),
    ok = typer_assert(typer(LenExp, TypeEnv, VarEnv) =:= int,
                      "array length should be int"),
    ok = typer_assert(typer(InitValueExp, TypeEnv, VarEnv) =:= TypeId,
                      "init value of array must match with array type"),
    list_to_atom(string:join([?PREFIX, atom_to_list(TypeId)], "")).

handle_typer_letexp({let_exp, _, Decs, Body}, TypeEnv, VarEnv) ->
    AllTypeDecs = lists:all(fun (Dec) ->
                                    element(1, Dec) =:= type_declaration
                            end,
                            Decs),
    {NewTypeEnv, NewVarEnv} = case AllTypeDecs of
                                  true ->
                                      {consume_type_decs(Decs, TypeEnv), VarEnv};
                                  false ->
                                      AllFuncDecs = lists:all(fun (Dec) ->
                                                                      element(1, Dec) =:= function_declaration
                                                              end,
                                                              Decs),
                                      case AllFuncDecs of
                                          true ->
                                              consume_func_decs(Decs, TypeEnv, VarEnv);
                                          false ->
                                              %% TODO support one let multi bindings
                                              AllVarDecs = lists:all(fun (Dec) ->
                                                                              element(1, Dec) =:= var_declaration
                                                                      end,
                                                                      Decs),
                                              ok = typer_assert(AllVarDecs,
                                                                "mixed def is not supported"),
                                              consume_var_decs(Decs, TypeEnv, VarEnv)
                                      end
                              end,
    lists:last([typer(E, NewTypeEnv, NewVarEnv)
                ||E <- Body]).

handle_typer_record_field({record_field, _, Exp, {identifier, _, Field}},
                          TypeEnv, VarEnv) ->
    Record_type_id = typer(Exp, TypeEnv, VarEnv),
    {type, {record, TFs}} = lookup_type(TypeEnv, Record_type_id),
    {Field, TypeId} = lists:keyfind(Field, 1, TFs),
    TypeId.

handle_typer_record_create_exp({record_create_exp, _, {identifier, _, RecordName},
                                TFs},
                               TypeEnv, VarEnv) ->
    {type, {record, FieldTypes}} = lookup_type(TypeEnv, RecordName),
    ok = lists:foreach(fun ({{FieldName, TypeId},
                             {{identifier, _, FieldName}, Exp}}) ->
                               InferType = typer(Exp, TypeEnv, VarEnv),
                               case InferType of
                                   nil ->
                                       FT = lookup_type(TypeEnv, TypeId),
                                       ok = case FT of
                                                {type, {record, _}} ->
                                                    ok;
                                                _ ->
                                                    erlang:error("nil should binding record")
                                            end;
                                   _ ->
                                       ok = typer_assert(InferType =:= TypeId,
                                                         "field type not match")
                               end
                       end,
                       lists:zip(FieldTypes, TFs)),
    RecordName.

%%
consume_type_decs(Decs, TypeEnv) ->
    % TODO: Empty Check
    TypeNames = [Tn
                 || {type_declaration, _, {identifier, _, Tn}, _} <- Decs],
    Types = build_types(Decs, TypeEnv, TypeNames),
    env:put(TypeEnv, lists:zip(TypeNames, Types)).

consume_func_decs(Decs, TypeEnv, VarEnv) ->
    FuncTypes = lists:map(fun get_func_type/1, Decs),
    FuncNames = lists:map(fun get_func_name/1, Decs),
    FuncTypeNames = lists:map(fun gen_func_type_name/1, Decs),
    NewTypeEnv = env:put(TypeEnv, lists:zip(FuncTypeNames, FuncTypes)),
    NewVarEnv = env:put(VarEnv, lists:zip(FuncNames, FuncTypeNames)),
    ok = lists:foreach(fun (Dec) ->
                               ok = verify_func_type(Dec, NewTypeEnv, NewVarEnv)
                       end,
                       Decs),
    {NewTypeEnv, NewVarEnv}.

consume_var_decs(Decs, TypeEnv, VarEnv) ->
    VarNames = [VarName 
                || {var_declaration, _, {identifier, _, VarName}, _, _} <- Decs],
    ExpTypes = [typer(Exp, TypeEnv, VarEnv)
                || {var_declaration, _, _, _, Exp} <- Decs],
    ExpectedTypes = [case ExpectType of
                         void -> void;
                         {identifier, _, Tid} -> Tid
                     end
                     || {var_declaration, _, _, ExpectType, _} <- Decs],
    ok = lists:foreach(fun ({ExpType, ExpectedType}) ->
                               ok = verify_var_type(ExpType, ExpectedType,
                                                    TypeEnv)
                       end,
                       lists:zip(ExpTypes, ExpectedTypes)),
    VarTypes = lists:map(fun merge_var_type/1,
                         lists:zip(ExpTypes, ExpectedTypes)),
    NewVarEnv = env:put(VarEnv, lists:zip(VarNames, VarTypes)),
    {TypeEnv, NewVarEnv}.

build_types([], _TypeEnv, _TypeNames) -> [];
build_types([{type_declaration, _, _, {identifier, _, Tid}}|Decs],
            TypeEnv, TypeNames) ->
    ok = typer_assert(true =:= env:contain(TypeEnv, Tid),
                      {"cannot find type name", Tid}),
    T = {type, {alias, Tid}},
    [T|build_types(Decs, TypeEnv, TypeNames)];
build_types([{type_declaration, _, _, {record_type, _, TFs}}|Decs],
            TypeEnv, TypeNames) ->
    ok = lists:foreach(fun ({{identifier, _, _},
                             {identifier, _, Tid}}) ->
                               InClause = lists:member(Tid, TypeNames),
                               InEnv = env:contain(TypeEnv, Tid),
                               ok = typer_assert(InClause or InEnv,
                                                 {"cannot find type name", Tid})
                       end,
                       TFs),
    T = {type, {record, [{Field, Tid}
                         || {{identifier, _, Field},
                             {identifier, _, Tid}} <- TFs]}},
    [T|build_types(Decs, TypeEnv, TypeNames)];
build_types([{type_declaration, _, _, {array_type, _, {identifier, _, Tid}}}|Decs],
            TypeEnv, TypeNames) ->
    InClause = lists:member(Tid, TypeNames),
    InEnv = env:contain(TypeEnv, Tid),
    ok = typer_assert(InClause or InEnv,
                      {"cannot find type name", Tid}),
    T = {type, {array, Tid}},
    [T|build_types(Decs, TypeEnv, TypeNames)].

get_func_type({function_declaration, _, _, Rtype, Paras, _}) ->
    ParaTypes = [Tid || {_, {identifier, _, Tid}} <- Paras],
    case Rtype of
        {identifier, _, RtypeId} ->
            {type, {function, ParaTypes, RtypeId}};
        void ->
            {type, {function, ParaTypes, void}}
    end.

get_func_name({function_declaration, _, {identifier, _, FuncName}, _, _, _}) ->
    FuncName.

gen_func_type_name({function_declaration, _, {identifier, _, FuncName}, _, _, _}) ->
    list_to_atom(string:join([atom_to_list(FuncName), "functype_id"], "_"));
gen_func_type_name(FuncName) when is_atom(FuncName) ->
    list_to_atom(string:join([atom_to_list(FuncName), "functype_id"], "_")).

verify_func_type(Dec={function_declaration, _, _, _, Paras, Exp},
                 TypeEnv, VarEnv) ->
    ParaTypes = [{Para, Tid}
                 || {{identifier, _, Para},
                     {identifier, _, Tid}}<- Paras],
    NewVarEnv = env:put(VarEnv, ParaTypes),
    {type, {function, _, Rt}} = lookup_type(TypeEnv, gen_func_type_name(Dec)),
    typer_assert(typer(Exp, TypeEnv, NewVarEnv) =:= Rt,
                 "function deftion type incorrect").

verify_var_type(nil, void, _TypeEnv) ->
    erlang:error("nil value declaration must specify type");
verify_var_type(nil, ExpectedType, TypeEnv) ->
    {type, {record, _}} = lookup_type(TypeEnv, ExpectedType),
    ok;    
verify_var_type(_ExpType, void, _TypeEnv) -> ok;
verify_var_type(ExpType, ExpectedType, TypeEnv) ->
    case ExpType =:= ExpectedType of
        true ->
            ok;
        false ->
            ok = typer_assert(lookup_type(ExpType, TypeEnv) =:=
                                  lookup_type(ExpectedType, TypeEnv),
                              "var define type error")
    end.

verify_para_arg(ParaType, nil, TypeEnv) ->
    {type, R} = lookup_type(TypeEnv, ParaType),
    typer_assert(element(1, R) =:= record,
                 "nil can only be binding to record");
verify_para_arg(ParaType, ArgType, TypeEnv) ->
    typer_assert(lookup_type(TypeEnv, ParaType) =:=
                     lookup_type(TypeEnv, ArgType),
                 "para arg type mismatch").

merge_var_type({ExpType, void}) -> ExpType;
merge_var_type({_, ExpectedType}) -> ExpectedType.

typer_assert(true, _) -> ok;
typer_assert(false, ErrMsg) ->
    erlang:error(ErrMsg).    

lookup_type(Env, TypeId) ->
    try
        env:lookup(Env, TypeId) 
    of
        V -> V
    catch _:_ ->
            S = atom_to_list(TypeId),
            L = length(?PREFIX),
            P = string:sub_string(S, 1, L),
            case list_to_atom(P) =:= list_to_atom(?PREFIX) of
                true ->
                    {type, {array, list_to_atom(string:sub_string(S, L+1))}};
                false ->
                    erlang:error("cannot find key")
            end
    end.
