-module(program4x_parse).

-include("type_defs.hrl").

-export([scan_and_parse/1]).

-spec scan_and_parse(string()) -> stm().
scan_and_parse(Code) ->
    {ok, Toks, _} = program4x_tok:string(Code),
    {Stm, []} = parse_stm(Toks),
    Stm.

-spec parse_stm(tokens()) -> {stm(), tokens()}.
parse_stm(Toks=[{id, _}|_]) ->
    {Stm, R} = parse_assign_stm(Toks),
    parse_stm_handle_rems(Stm, R);
parse_stm(Toks=[{keyword, print}|_]) ->
    {Stm, R} = parse_print_stm(Toks),
    parse_stm_handle_rems(Stm, R).

-spec parse_stm_handle_rems(stm(), tokens()) -> {stm(), tokens()}.
parse_stm_handle_rems(Stm, [{symbol, ';'}|R]) ->
    {Stm1, R1} = parse_stm(R),
    {{compound_stm, Stm, Stm1}, R1};
parse_stm_handle_rems(Stm, R) -> {Stm, R}.

-spec parse_assign_stm(tokens()) -> {stm(), tokens()}.
parse_assign_stm([{id, Id}, {symbol, ':='}|R]) ->
    {Exp, R1} = parse_exp(R), 
    Stm = {assign_stm, Id, Exp},
    {Stm, R1}.

-spec parse_print_stm(tokens()) -> {stm(), tokens()}.
parse_print_stm([{keyword, print}, {symbol, '('}|R]) ->
    {Exps, [{symbol, ')'}|R1]} = parse_multi(R, fun parse_exp/1, {symbol, ','}),
    Stm = {print_stm, Exps},
    {Stm, R1}.

-spec parse_multi(tokens(), fun((tokens())->E), token()) -> [E].
parse_multi(Toks, Fun, Delim) ->
    parse_multi(Toks, Fun, Delim, []).

parse_multi(Toks, Fun, Delim, Acc) ->
    try Fun(Toks) of
        {A, R} ->
            case R of
                [Delim|R1] ->
                    parse_multi(R1, Fun, Delim, [A|Acc]);
                _ ->
                    {lists:reverse([A|Acc]), R}
            end
    catch
        _:_ ->
            {lists:reverse(Acc), Toks}
    end.

-spec parse_exp(tokens()) -> {exp(), tokens()}.
parse_exp([{id, Id}|R]) ->
    Exp = {id_exp, Id},
    parse_exp_handle_rems(Exp, R);
parse_exp([{int, Num}|R]) ->
    Exp = {num_exp, Num},
    parse_exp_handle_rems(Exp, R);
parse_exp([{symbol, '('}|R]) ->
    {Stm, [{symbol, ','}|R1]} = parse_stm(R),
    {Exp, [{symbol, ')'}|R2]} = parse_exp(R1),
    parse_exp_handle_rems({eseq_exp, Stm, Exp}, R2).

-spec parse_exp_handle_rems(exp(), tokens()) -> {exp(), tokens()}.
parse_exp_handle_rems(Exp1, [{symbol, Op}|R]) when Op =:= '+';
                                                   Op =:= '-';
                                                   Op =:= '*';
                                                   Op =:= '/' ->
    {Exp2, R1} = parse_exp(R),
    {{op_exp, Exp1, Op, Exp2}, R1};
parse_exp_handle_rems(Exp, R) -> {Exp, R}.

