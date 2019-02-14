Definitions.


Rules.

;                        : {token, {symbol, ';'}}.
:=                       : {token, {symbol, ':='}}.
,                        : {token, {symbol, ','}}.
print                    : {token, {keyword, print}}.
\+                       : {token, {symbol, '+'}}.
-                        : {token, {symbol, '-'}}.
\*                       : {token, {symbol, '*'}}.
\/                       : {token, {symbol, '/'}}.
\(                       : {token, {symbol, '('}}.
\)                       : {token, {symbol, ')'}}.

[a-zA-Z][a-zA-Z0-9]*     : {token, {id, list_to_atom(TokenChars)}}.
[0-9][0-9]*              : {token, {int, list_to_integer(TokenChars)}}.

%% Space to ignore
\t                       : skip_token.
\n                       : skip_token.
\r                       : skip_token.
\s                       : skip_token.

Erlang code.
