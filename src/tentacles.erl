-module(tentacles).

-export([execute/3, execute/4, send/2, send/3, ping/1, die/1, suicide/1,
         get_state/1, hail/2, dismiss/1]).

-define(TENTACLES_SENDER, sender).

-define(TENTACLES_REDISTRIBUTOR, redistributor).

%-------------------------------------------------------------------------------
% Types.
%-------------------------------------------------------------------------------

-type program() :: atom().
%% Program to be executed.

-type arguments() :: list(term()).
%% Arguments for the program.

-type error() :: {error, timeout}
               | {error, unavailable}
               | {error, unknown}
               | {error, term()}.
%% General error.

-type program_state() :: {normal | emergency, running | pending | down | kill}.
%% Program state.

%-------------------------------------------------------------------------------
% Public functions.
%-------------------------------------------------------------------------------

-spec execute( Id        :: tentacles_disptacher:id()
             , Program   :: program()
             , Arguments :: arguments()) -> {ok, tentacles_dispatcher:microsecs()}
                                          | error().
%% @doc Executes a remote `Program` (should be an atom) using its `Arguments`.
%% The program is differentiated among the rest of the same instance of the
%% program using an `Id` that must be unique.
execute(Id, Program, Args) ->
    execute(Id, Program, Args, []).

-spec execute( Id        :: tentacles_dispatcher:id()
             , Program   :: program()
             , Arguments :: arguments()
             , BadNodes  :: [node()]) -> {ok, tentacles_dispatcher:microsecs()}
                                       | error().
%% @doc Same as execute/3 but it also receives a list of nodes. These `BadNodes`
%% are forbbiden nodes for execution.
execute(Id, Program, Args, BadNodes) ->
    send_to_sender(Id, {exec, Program, Args, BadNodes}).

-spec send( Id      :: tentacles_dispatcher:id()
          , Message :: tentacles_controller:message()) ->
                      {{ok, term()}, tentacles_dispatcher:microsecs()}
                    | error().
%% @doc Sends a `Message` to a program using its `Id`.
send(Id, Message) ->
    send_to_sender(Id, {send, Message}).

-spec send( Id      :: tentacles_dispatcher:id()
          , Sender  :: tentacles_dispatcher:id()
          , Message :: tentacles_controller:message()) ->
                      {{ok, term()}, tentacles_dispatcher:microsecs()}
                    | error().
%% @doc Sends a `Message` to a program by its `Id` from another program identified
%% by its identifier `Sender`.
send(Id, Sender, Message) ->
    send_to_sender(Id, {send, Sender, Message}).

-spec ping(Id :: tentacles_dispatcher:id()) ->
                  {pong, tentacles_dispatcher:microsecs()}
                | pang.
%% @doc Pings program by `Id`.
ping(Id) ->
    case send_to_sender(Id, ping) of
        {error, _} -> pang;
        Response   -> Response
    end.

-spec die(Id :: tentacles_dispatcher:id()) ->
                  {ok, tentacles_dispatcher:microsecs()}
                | error().
%% @doc Kills a program by `Id`.
die(Id) ->
    send_to_sender(Id, die).

-spec suicide(Id :: tentacles_dispatcher:id()) ->
                  {ok, tentacles_dispatcher:microsecs()}
                | error().
%% @doc Informs the server that the program identified by `Id` shuts itself down.
suicide(Id) ->
    send_to_sender(Id, suicide).

-spec get_state(Id :: tentacles_dispatcher:id()) ->
                      {program_state(), tentacles_dispatcher:microsecs()}
                    | error().
%% @doc Gets state of a program by its `Id`.
get_state(Id) ->
    send_to_sender(Id, get_state).

-spec hail( MasterNode :: node()
          , MaxLoad    :: non_neg_integer()) ->
                { {ok, Priority :: integer()}, tentacles_dispatcher:microsecs()}
              | error().
% @doc Informs availability of the caller node.
hail(MasterNode, MaxLoad) ->
    send_to_redistributor(MasterNode, {hail, MaxLoad}).

-spec dismiss(MasterNode :: node()) -> {ok, tentacles_dispatcher:microsecs()}
                                     | error().
% @doc Informs unavailability of certain server (shutdown).
dismiss(MasterNode) ->
    send_to_redistributor(MasterNode, dismiss).

%-------------------------------------------------------------------------------
% Private functions.
%-------------------------------------------------------------------------------

-spec send_to_sender( Id :: tentacles_dispatcher:id()
                    , Message :: tentacles_controller:message()) ->
                            tentacles_dispatcher:response().
%% @doc Send message to tentacles sender server.
send_to_sender(Id, Message) ->
    Response = tentacles_dispatcher:concurrent_message( ?TENTACLES_SENDER
                                                      , node()
                                                      , Id
                                                      , Message),
    case Response of
        {{error, _} = Error, _} -> Error;
        {_, _}                  -> Response
    end.

-spec send_to_redistributor( MasterNode :: node()
                           , Message    :: tentacles_controller:message()) ->
                                    tentacles_dispatcher:response().
%% @doc Sends `Message` to redistributor at `MasterNode`.
send_to_redistributor(MasterNode, Message) ->
    Response = tentacles_dispatcher:sync_message( ?TENTACLES_REDISTRIBUTOR
                                                , MasterNode
                                                , node()
                                                , Message),
    case Response of
        {{error, _} = Error, _} -> Error;
        _                       -> Response
    end.
