-module(tentacles_sender_controller).

-behaviour(tentacles_controller).

% Callbacks.
-export([init/2, handle_message/2, handle_timeout/1, handle_event/2,
         handle_termination/2]).

-record(state, { base_name :: tentacles_dispatcher:base_name()
               , id        :: tentacles_dispatcher:id()}).

-define(TENTACLES_RECEIVER, receiver).

%-------------------------------------------------------------------------------
% Callback implementation.
%-------------------------------------------------------------------------------

init(BaseName, Id) ->
    State = #state{ base_name = BaseName
                  , id        = Id},
    {ok, State}.

% ok | {error, Reason}
handle_message({exec, Program, Args, BadNodes}, State) ->
    Response = remote_command(State, BadNodes, {exec, Program, Args}),
    {reply, Response, State};

% {ok, Response} | {error, Reason}
handle_message({send, Message}, State) ->
    Response = remote_command(State, [], {send, Message}),
    {reply, Response, State};

% {ok, Response} | {error, Reason}
handle_message({send, Sender, Message}, State) ->
    case is_it_here(Sender) of
        true ->
            Response = remote_command(State, [], {send, Message}),
            {reply, Response, State};
        false ->
            {reply, {error, not_here}, State}
    end;

% pong | pang
handle_message(ping, State) ->
    Response = remote_command(State, [], ping),
    {reply, Response, State};

% ok | {error, Reason}
handle_message(die, State) ->
    Response = remote_command(State, [], die),
    {reply, Response, State};

% ok | {error, Reason}
handle_message(suicide, State) ->
    Id = State#state.id,
    case is_it_here(Id) of
        true ->
            Response = delete_id(Id),
            {reply, Response, State};
        false ->
            {reply, {error, not_here}, State}
    end;

% {normal | emergency, running | pending | down | kill} | {error, Reason}.
handle_message(get_state, State) ->
    Id = State#state.id,
    Response = get_state(Id),
    {reply, Response, State};

% Any.
handle_message(_Any, State) ->
    {noreply, State}.

handle_timeout(State) ->
    {noreply, State}.

handle_event(_Event, State) ->
    {noreply, State}.

handle_termination(_Reason, _State) ->
    ok.

%-------------------------------------------------------------------------------
% Private functions.
%-------------------------------------------------------------------------------

-spec remote_command( State    :: #state{}
                    , BadNodes :: list(node())
                    , Message  :: term()) -> term().
%% @doc Executes remote command.
remote_command(State, BadNodes, Message) ->
    case pick_node(BadNodes) of
        none ->
            {error, busy_server};
        Node ->
            Id       = State#state.id,
            Response = tentacles_dispatcher:async_message( ?TENTACLES_RECEIVER
                                                         , Node
                                                         , Id
                                                         , Message),
            case Response of
                {error, _} = Error ->
                    Error;
                {Reply, _}         ->
                    Reply    
            end
    end.

-spec pick_node(BadNodes :: list(node())) -> none | node().
%% @doc TODO: Picks a node: a. Less loaded node if Id does not exist. b. Node where Id
%%      is running. c. A node different from any node in BadNodes.
pick_node(_BadNodes) ->
    node().

-spec is_it_here(Id :: tentacles_dispatcher:id()) -> true | false.
%% @doc TODO:Whether the Id is in this node or not, according to the database.
is_it_here(_Id) ->
    true.

-spec delete_id(Id :: tentacles_dispatcher:id()) -> ok.
%% @doc TODO: Deletes an Id from the database.
delete_id(_Id) ->
    ok.

-spec get_state(Id :: tentacles_dispatcher:id()) -> {normal | emergency, running | pending | down | kill}
                                                  | {error, Reason :: term()}.
%% @doc TODO: Gets Id's state from database.
get_state(_Id) ->
    {normal, running}.
