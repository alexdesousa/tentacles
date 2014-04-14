-module(tentacles_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NUMBER_OF_TESTS, 100).

server_test() ->
    application:set_env(tentacles, tentacles_hooks, enode),
    application:set_env(tentacles, tentacles_enode_controller_max_age, infinity),
    application:start(tentacles),
    enode:start(),
    test(),
    application:stop(tentacles).

test() ->
    {ok, Ids} = proper_gen:pick(ids(), ?NUMBER_OF_TESTS),
    ?assertEqual(true, proper:quickcheck(?MODULE:prop_execute(), [{to_file, user}])),
    response_time(Ids, execute, [program, [a,b,c]]),

    ?assertEqual(true, proper:quickcheck(?MODULE:prop_send(), [{to_file, user}])),
    response_time(Ids, send, [message]),

    ?assertEqual(true, proper:quickcheck(?MODULE:prop_ping(), [{to_file, user}])),
    response_time(Ids, ping, []),

    ?assertEqual(true, proper:quickcheck(?MODULE:prop_die_suicide(), [{to_file, user}])),
    response_time(Ids, die, []).

response_time(Ids, Function, Args) ->
    response_time(Ids, Function, Args, 0, length(Ids)).

response_time([], Function, Args, Acc, N) ->
    ?debugFmt("~s/~p average time: ~.2f μs", [Function, length(Args) + 1, Acc/N]);
response_time([Id | Ids], Function, Args, Acc, N) ->
    case erlang:apply(tentacles, Function, [Id | Args]) of
        {error, _} ->
            response_time(Ids, Function, Args, Acc, N - 1);
        {_, Ms}    ->
            response_time(Ids, Function, Args, Acc + Ms, N)
    end.

prop_execute() ->
    ?FORALL(Id, id(),
        begin
            Response = case tentacles:execute(Id, program, [a,b,c]) of
                {ok, _} ->
                    true;
                {error, _} ->
                    false
            end,
            tentacles:die(Id),
            Response
        end
    ).

prop_send() ->
    ?FORALL(Id, id(),
        begin
            Exec = case tentacles:execute(Id, program, [a,b,c]) of
                {ok, _} ->
                    true;
                {error, _} ->
                    false
            end,
            Response = case {Exec, tentacles:send(Id, message)} of
                {true, {{ok, _}, _}} ->
                    true;
                {true, {error, no_such_process}} ->
                    false;
                {false, {error, no_such_process}} ->
                    true;
                {false, _} ->
                    false
            end,
            tentacles:die(Id),
            Response
        end
    ).

prop_ping() ->
    ?FORALL(Id, id(),
        begin
            Exec = case tentacles:execute(Id, program, [a,b,c]) of
                {ok, _} ->
                    true;
                {error, _} ->
                    false
            end,
            Response = case {Exec, tentacles:ping(Id)} of
                {true, {pong, _}} ->
                    true;
                {true, {pang, _}} ->
                    false;
                {false, {pang, _}} ->
                    true;
                {false, _} ->
                    false
            end,
            tentacles:die(Id),
            Response
        end
    ).

prop_die_suicide() ->
    ?FORALL(Id, id(),
        begin
            tentacles:execute(Id, program, [a,b,c]),
            case tentacles:die(Id) of
                {ok, _} ->
                    case tentacles:ping(Id) of
                        {pong, _} ->
                            false;
                        {pang, _} ->
                            true
                    end;
                _ ->
                    false
            end
        end
    ).

id() -> integer().

ids() -> list(integer()).
