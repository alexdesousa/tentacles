-module(tentacles_hooks).

-export([execute_hook/2]).

-define(HOOKS, tentacles_hooks).
-define(DEFAULT_HOOKS, tentacles_default_hooks).

%-------------------------------------------------------------------------------
% Callbacks.
%-------------------------------------------------------------------------------

%% @doc Callback to execute a program.
-callback execute( Id      :: term()
                 , Program :: atom()
                 , Args    :: list(term())) -> ok
                                             | {error, Reason :: term()}.

%% @doc Callback to send message to a process.
-callback send( Id      :: term()
              , Message :: term()) -> {ok,    Response :: term()}
                                    | {error, Reason   :: term()}.

%% @doc Callback to ping a process.
-callback ping(Id :: term()) -> pong | pang.

%% @doc Callback to send a die signal to a process.
-callback die(Id :: term()) -> ok | {error, Reason :: term()}.

%% @doc Callback to send a whois to every process to get its Id.
-callback whois_broadcast() -> {ok, Ids :: list(term())}. 

%-------------------------------------------------------------------------------
% Helper functions.
%-------------------------------------------------------------------------------

-spec execute_hook( Function :: atom()
                  , Args :: list(term())) -> {error, no_function}
                                           | {error, no_module}
                                           | {error, bad_arity}
                                           | term().
%% @doc Executes functions if the hook module exists.
execute_hook(Function, Args) ->
    Module = case application:get_env(tentacles, ?HOOKS) of
        undefined -> ?DEFAULT_HOOKS;
        {ok, M}   -> M
    end,
    try Module:module_info(exports) of
        Prop ->
            Arity = length(Args),
            case proplists:get_value(Function, Prop) of
                undefined ->
                    {error, no_function};
                Arity ->
                    erlang:apply(Module, Function, Args);
                _     ->
                    {error, badarity}
            end
    catch
        _:_ ->
            {error, no_module}
    end.
