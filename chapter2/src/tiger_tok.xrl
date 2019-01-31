Definitions.

L = [a-zA-Z]
D = [0-9]

Rules.

\/\*[a-zA-Z0-9\t\n\r\s_!.,:*\-]*\*\/      : {token, {comments}}.

=                                         : {token, {symbol, '='}}.
>=                                        : {token, {symbol, '>='}}.
<=                                        : {token, {symbol, '<='}}.
<                                         : {token, {symbol, '<'}}.
>                                         : {token, {symbol, '>'}}.
<>                                        : {token, {symbol, '<>'}}.
[+\-*/]                                   : {token, {symbol, list_to_atom(TokenChars)}}.
[|&]                                      : {token, {symbol, list_to_atom(TokenChars)}}.

[()\[\]{},.:;]                            : {token, {symbol, list_to_atom(TokenChars)}}.

type                                      : {token, {keyword, type}}.
array                                     : {token, {keyword, array}}.
of                                        : {token, {keyword, 'of'}}.
int                                       : {token, {keyword, int}}.
string                                    : {token, {keyword, string}}.
nil                                       : {token, {keyword, nil}}.

var                                       : {token, {keyword, var}}.
:=                                        : {token, {symbol, ':='}}.

function                                  : {token, {keyword, function}}.

let                                       : {token, {keyword, 'let'}}.
in                                        : {token, {keyword, 'in'}}.
end                                       : {token, {keyword, 'end'}}.

if                                        : {token, {keyword, 'if'}}.
then                                      : {token, {keyword, 'then'}}.
else                                      : {token, {keyword, 'else'}}.

while                                     : {token, {keyword, 'while'}}.
do                                        : {token, {keyword, 'do'}}.
for                                       : {token, {keyword, 'for'}}.
to                                        : {token, {keyword, 'to'}}.
break                                     : {token, {keyword, 'break'}}.


{L}({L}|{D}|_)*                           : {token, {identifier, list_to_atom(TokenChars)}}.
{D}                                       : {token, {integer, list_to_integer(TokenChars)}}.
[1-9]{D}*                                 : {token, {integer, list_to_integer(TokenChars)}}.
"([^"\\]|\\.)*"                           : {token, {string, transform_to_erlang_string(string:strip(TokenChars, both, $\"))}}.


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
