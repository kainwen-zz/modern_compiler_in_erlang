Definitions.

L = [a-zA-Z]
D = [0-9]

Rules.

\/\*[a-zA-Z0-9\t\n\r\s_!.,:*\-]*\*\/      : skip_token.

=                                         : {token, {'=', TokenLine}}.
>=                                        : {token, {'>=', TokenLine}}.
<=                                        : {token, {'<=', TokenLine}}.
<                                         : {token, {'<', TokenLine}}.
>                                         : {token, {'>', TokenLine}}.
<>                                        : {token, {'<>', TokenLine}}.
[+\-*/]                                   : {token, {list_to_atom(TokenChars), TokenLine}}.
[|&]                                      : {token, {list_to_atom(TokenChars), TokenLine}}.

[()\[\]{},.:;]                            : {token, {list_to_atom(TokenChars), TokenLine}}.

type                                      : {token, {type, TokenLine}}.
array                                     : {token, {array, TokenLine}}.
of                                        : {token, {'of', TokenLine}}.
nil                                       : {token, {nil, TokenLine}}.

var                                       : {token, {var, TokenLine}}.
:=                                        : {token, {':=', TokenLine}}.

function                                  : {token, {function, TokenLine}}.

let                                       : {token, {'let', TokenLine}}.
in                                        : {token, {'in', TokenLine}}.
end                                       : {token, {'end', TokenLine}}.

if                                        : {token, {'if', TokenLine}}.
then                                      : {token, {'then', TokenLine}}.
else                                      : {token, {'else', TokenLine}}.

while                                     : {token, {'while', TokenLine}}.
do                                        : {token, {'do', TokenLine}}.
for                                       : {token, {'for', TokenLine}}.
to                                        : {token, {'to', TokenLine}}.
break                                     : {token, {'break', TokenLine}}.


{L}({L}|{D}|_)*                           : {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.
{D}                                       : {token, {integer_const, TokenLine, list_to_integer(TokenChars)}}.
[1-9]{D}*                                 : {token, {integer_const, TokenLine, list_to_integer(TokenChars)}}.
"([^"\\]|\\.)*"                           : {token, {string_const, TokenLine, transform_to_erlang_string(string:strip(TokenChars, both, $\"))}}.


%% Space to ignore
\t                             : skip_token.
\n                             : skip_token.
\r                             : skip_token.
\s                             : skip_token.

Erlang code.

-spec transform_to_erlang_string(string()) -> string().
transform_to_erlang_string([]) -> [];
transform_to_erlang_string([Char|RemString]) when Char /= $\\ ->
    [Char|transform_to_erlang_string(RemString)];
transform_to_erlang_string([$\\, $n|RemString]) ->
    [$\n|transform_to_erlang_string(RemString)];
transform_to_erlang_string([$\\, $t|RemString]) ->
    [$\t|transform_to_erlang_string(RemString)];
transform_to_erlang_string([$\\, $^, Char|RemString]) ->
    ControlChar = [$@, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z, $[, $\\, $], $^, $?],
    Map = lists:zip(ControlChar, lists:seq(0, 31)),
    % TODO: report error
    {_, Ascii} = lists:keyfind(Char, 1, Map),
    [Ascii|transform_to_erlang_string(RemString)];
transform_to_erlang_string([$\\, $\"|RemString]) ->
    [$\"|transform_to_erlang_string(RemString)];
transform_to_erlang_string([$\\, $\\|RemString]) ->
    [$\\|transform_to_erlang_string(RemString)];
transform_to_erlang_string([$\\, Char1, Char2, Char3|RemString]) ->
    Oct = [Char1, Char2, Char3],
    Int = (catch erlang:list_to_integer(Oct)),
    case is_integer(Int) of
        true ->
            [Int|transform_to_erlang_string(RemString)];
        false ->
            R1 = lists:dropwhile(fun (C) ->
                                         lists:member(C,
                                                      [$\n, $\t, 32, $\r])
                                 end,
                                 [Char1, Char2, Char3|RemString]),
            [$\\|R2] = R1,
            transform_to_erlang_string(R2)
    end.