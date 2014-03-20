-module(tentacles_sender_controller).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { id      :: any()
               , max_age :: tentacles_server:seconds()
               , parent  :: atom()
               }).

start_link(Id, Parent, MaxAge) ->
    MaxAgeMs = MaxAge * 1000,
    gen_server:start_link(?MODULE, [Id, Parent, MaxAgeMs], []).

init([Id, Parent, MaxAge]) ->
    {ok, #state{id = Id, max_age = MaxAge, parent = Parent}, MaxAge}.

handle_call(_, _From, State) ->
    {noreply, State, State#state.max_age}.

% {ok, NewPriority} | {error, Reason}
handle_cast({{up, MasterNode, Node, Priority, Nll, Ell}, From}, State) ->
    gen_server:reply(From, {ok, Priority}),
    {noreply, State, State#state.max_age};

% ok | {error, Reason}
handle_cast({{down, MasterNode, Node}, From}, State) ->
    gen_server:reply(From, ok),
    {noreply, State, State#state.max_age};

% pong | pang.
handle_cast({{ping_server, Node}, From}, State) ->
    case send_to_receiver(Node, internal, ping) of
        {pong, _} = Pong ->
            gen_server:reply(From, Pong);
        {{error, _}, Ms} ->
            gen_server:reply(From, {pang, Ms})
    end,
    {noreply, State, State#state.max_age};

% ok | {error, Reason}
handle_cast({{exec, Id, Program, Args, BadNodes}, From}, State) ->
    gen_server:reply(From, ok),
    {noreply, State, State#state.max_age};
    
% {ok, Response} | {error, Reason}
handle_cast({{send, Id, Message}, From}, State) ->
    gen_server:reply(From, {ok, response}),
    {noreply, State, State#state.max_age};

% {ok, Response} | {error, Reason}
handle_cast({{send, SenderId, Id, Message}, From}, State) ->
    gen_server:reply(From, {ok, response}),
    {noreply, State, State#state.max_age};

% ok | {error, Reason}
handle_cast({{die, Id}, From}, State) ->
    gen_server:reply(From, ok),
    {noreply, State, State#state.max_age};

% ok | {error, Reason}
handle_cast({{suicide, Id}, From}, State) ->
    gen_server:reply(From, ok),
    {noreply, State, State#state.max_age};

% pong | pang.
handle_cast({{ping, Id}, From}, State) ->
    gen_server:reply(From, pong),
    {noreply, State, State#state.max_age};

% {normal | emergency, running | pending | down | kill} | {error, Reason}.
handle_cast({{get_state, Id}, From}, State) ->
    gen_server:reply(From, running),
    {noreply, State, State#state.max_age}.

handle_info(timeout, State) ->
    gen_server:cast(State#state.parent, {expire, State#state.id}),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State, State#state.max_age}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec send_to_receiver( Node :: local | node()
                      , Id   :: any()
                      , Msg  :: any()) -> {any(), tentacles_server:millisecs()}.
%% @doc Sends message `Msg` to tentacles_receiver in the specified `Node` to the 
%% process with identified by `Id`.
send_to_receiver(Node, Id, Msg) ->
    tentacles_server:send_to_server(Node, tentacles_receiver, {Id, Msg}).

-spec send_to_redistributor( Node :: local | node()
                           , Msg  :: any()) -> {any(), tentacles_server:millisecs()}.
%% @doc Sends message `Msg` to tentacles_redistributor in the specified `Node`.
send_to_redistributor(Node, Msg) ->
    % Get redistributor node? Probably.
    tentacles_server:send_to_server(Node, tentacles_redistributor, {serial, Msg}).%TODO: serial calls in tentacles_server.
