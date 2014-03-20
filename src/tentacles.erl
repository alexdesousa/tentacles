-module(tentacles).

-export([execute/3, execute/4, send/2, send/3, die/1, suicide/1, ping/1, get_state/1,
         inform_availability/4, inform_unavailability/1, ping_server/1]).

-spec execute( Id        :: any()
             , Program   :: atom()
             , Arguments :: [any()]) -> { ok
                                        | {error, Reason :: any()}
                                        , tentacles_server:millisecs()}.
%% @doc Executes a remote `Program` (should be an atom) using its `Arguments`.
%% The program is differentiated among the rest of the same instance of the
%% program using an `Id` that must be unique.
execute(Id, Program, Args) ->
    execute(Id, Program, Args, []).

-spec execute( Id        :: any()
             , Program   :: atom()
             , Arguments :: [any()]
             , BadNodes  :: [node()]) -> { ok
                                         | {error, Reason :: any()}
                                         , tentacles_server:millisecs()}.
%% @doc Same as execute/3 but it also receives a list of nodes. These `BadNodes`
%% are forbbiden nodes for execution.
execute(Id, Program, Args, BadNodes) ->
    send_to_sender(Id, {exec, Id, Program, Args, BadNodes}).

-spec send( Id      :: any()
          , Message :: any()) -> { {ok, Response :: any()}
                                 | {error, Reason :: any()}
                                 , tentacles_server:millisecs()}.
%% @doc Sends a `Message` to a program using its `Id`.
send(Id, Message) ->
    send_to_sender(Id, {send, Id, Message}).

-spec send( Id      :: any()
          , OwnId   :: any()
          , Message :: any()) -> { {ok, Response :: any()}
                                 | {error, Reason :: any()}
                                 , tentacles_server:millisecs()}.
%% @doc Sends a `Message` to a program by its `Id` from another program identified
%% by its `OwnId`.
send(Id, OwnId, Message) ->
    send_to_sender(Id, {send, OwnId, Id, Message}).

-spec die(Id :: any()) -> { ok
                          | {error, Reason :: any()}
                          , tentacles_server:millisecs()}.
%% @doc Kills a program by `Id`.
die(Id) ->
    send_to_sender(Id, {die, Id}).

-spec suicide(Id :: any()) -> { ok
                              | {error, Reason :: any()}  
                              , tentacles_server:millisecs()}.
%% @doc Informs the server that the program identified by `Id` shuts itself down.
suicide(Id) ->
    send_to_sender(Id, {suicide, Id}).

-spec ping(Id :: any()) -> {pong | pang, tentacles_server:millisecs()}.
%% @doc Pings program by `Id`.
ping(Id) ->
    send_to_sender(Id, {ping, Id}).

-spec get_state(Id :: any()) -> { { LocationState :: normal | emergency
                                  , RunningState  :: running | pending | down | kill}
                                | {error, Reason :: any()}
                                , tentacles_server:millisecs()}.
%% @doc Gets state of a program by its `Id`.
get_state(Id) ->
    send_to_sender(Id, {get_state, Id}).

-spec inform_availability( MasterNode :: node()
                         , Priority   :: integer()
                         , Nll        :: non_neg_integer()
                         , Ell        :: non_neg_integer()) ->
                                { {ok, Priority :: integer()}
                                | {error, Reason :: any()}
                                , tentacles_server:millisecs()}.
% @doc Informs availability of certain server.
inform_availability(MasterNode, Priority, Nll, Ell) ->
    if
        Nll > Ell ->
            {error, wrong_limits};
        true ->
            send_to_sender({up, MasterNode, node(), Priority, Nll, Ell})
    end.

-spec inform_unavailability(MasterNode :: node()) -> { ok
                                                   | {error, Reason :: any()}
                                                   , tentacles_server:millisecs()}.
% @doc Informs unavailability of certain server (shutdown).
inform_unavailability(MasterNode) ->
    send_to_sender({down, MasterNode, node()}).

-spec ping_server(Node :: node()) -> {pong | pang, tentacles_server:millisecs()}.
% @doc Pings remote server.
ping_server(Node) ->
    send_to_sender({ping_server, Node}).

%% @doc Function to send messages to the tentacles_server sub-process redistributor.
send_to_sender(Msg) ->
    tentacles_server:send_to_server(local, tentacles_sender, {internal, Msg}).

%% @doc Function to send messages to tentacles_sender process.    
send_to_sender(Id, Msg) ->
    case Id of
        internal ->
            {error, wrong_id};
        _ ->
            tentacles_server:send_to_server(local, tentacles_sender, {Id, Msg})
    end.
