-module(tentacles).

-export([execute/3, execute/4, send/2, send/3, die/1, suicide/1, ping/1, get_state/1]).

-spec execute( Id        :: any()
             , Program   :: atom()
             , Arguments :: [any()]) -> ok | {error, Reason}.
%% @doc Executes a remote `Program` (should be an atom) using its `Arguments`.
%% The program is differentiated among the rest of the same instance of the
%% program using an `Id` that must be unique.
execute(Id, Program, Args) ->
    execute(Id, Program, Args, []).

-spec execute( Id        :: any()
             , Program   :: atom()
             , Arguments :: [any()],
             , BadNodes  :: [node()]) -> ok | {error, Reason}.
%% @doc Same as execute/3 but it also receives a list of nodes. These `BadNodes`
%% are forbbiden nodes for execution.
execute(Id, Program, Args, BadNodes) ->
    gen_server:call(tentacles_sender, {exec, Id, Program, Args, BadNodes}).

-spec send( Id      :: any()
          , Message :: any()) -> {ok, Response} | {error, Reason}.
%% @doc Sends a `Message` to a program using its `Id`.
send(Id, Message) ->
    gen_server:call(tentacles_sender, {send, Id, Message}).

-spec send( Id      :: any()
          , OwnId   :: any()
          , Message :: any()) -> {ok, Response} | {error, Reason}.
%% @doc Sends a `Message` to a program by its `Id` from another program identified
%% by its `OwnId`.
send(Id, OwnId, Message) ->
    gen_server:call(tentacles_sender, {send, Id, OwnId, Message}).

-spec die(Id :: any()) -> ok | {error, Reason}.
%% @doc Kills a program by `Id`.
die(Id) ->
    gen_server:call(tentacles_sender, {die, Id}).

-spec suicide(Id :: any()) -> ok | {error, Reason}.
%% @doc Informs the server that the program identified by `Id` shuts itself down.
suicide(Id) ->
    gen_server:call(tentacles_sender, {suicide, Id}).

-spec ping(Id :: any()) -> pong | pang.
%% @doc Pings program by `Id`.
ping(Id) ->
    gen_server:call(tentacles_sender, {ping, Id}).

-spec get_state(Id :: any()) -> any() | {error, Reason}.
%% @doc Gets state of a program by its `Id`.
get_state(Id) ->
    gen_server:call(tentacles_sender, {get_state, Id}).
