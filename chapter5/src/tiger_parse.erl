-module(tiger_parse).

-include("tiger_syntax_tree.hrl").

-export([scan_and_parse/1, scan_and_parse_file/1]).

-spec scan_and_parse_file(Fn::string()) -> program().
scan_and_parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    scan_and_parse(Code).

-spec scan_and_parse(Code::string()) -> program().
scan_and_parse(Code) ->
    {ok, Toks, _} = tiger_tok:string(Code),
    {ok, SyntaxTree} = tiger_grammar:parse(Toks ++ [{'$end', 1}]),
    SyntaxTree.
