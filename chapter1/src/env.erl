-module(env).

-export([init/0, extend_env/3, apply_env/2]).

-type value() :: integer().

-opaque env() :: [{Key::string(), Value::value()}].

-export_type([env/0]).

-spec init() -> env().
init() ->
    [].

-spec extend_env(env(), Key::string(), Value::value()) -> env().
extend_env(Env, Key, Value) ->
    [{Key, Value}|Env].

-spec apply_env(env(), Key::string()) -> value().
apply_env(Env, Key) ->
    case lists:keyfind(Key, 1, Env) of
        {Key, Value} ->
            Value;
        false ->
            erlang:error({key_not_found, Env, Key})
    end.
