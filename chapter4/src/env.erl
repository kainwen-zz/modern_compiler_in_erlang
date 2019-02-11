-module(env).

-behaviour(gen_server).

-export([start/0, extend_env/2, apply_env/1]).

-export([init/1, handle_call/3, handle_cast/2]).

-type value() :: integer().

-spec start() -> ok.
start() ->
    {ok, _} = gen_server:start_link({local, env}, ?MODULE, [], []),
    ok.

-spec extend_env(Key::string(), Value::value()) -> ok.
extend_env(Key, Value) ->
    ok = gen_server:call(env, {extend_env, Key, Value}).

-spec apply_env(Key::string()) -> value().
apply_env(Key) ->
    gen_server:call(env, {apply_env, Key}).

% callbacks
init([]) ->
    {ok, []}.

handle_call({extend_env, Key, Value}, _From, State) ->
    NewState = [{Key, Value}|State],
    {reply, ok, NewState};
handle_call({apply_env, Key}, _From, State) ->
    V = case lists:keyfind(Key, 1, State) of
            {Key, Value} ->
                Value;
            false ->
                erlang:error({key_not_found, State, Key})
        end,
    {reply, V, State}.

handle_cast(_, State) ->
    {noreply, State}.
    
