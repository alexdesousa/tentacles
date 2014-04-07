-module(tentacles_receiver_controller).

-behaviour(tentacles_controller).

-export([start_link/4]).

% Callbacks.
-export([init/1, handle_message/2, handle_timeout/1, handle_event/2,
         handle_termination/2]).

-record(state, { base_name :: tentacles_dispatcher:base_name()
               , id        :: tentacles_dispatcher:id()}).

start_link(BaseName, Controller, Id, MaxAge) ->
    tentacles_controller:start_link(Controller, [BaseName, Id, MaxAge]).

init([BaseName, Id, MaxAge]) ->
    State = #state{ base_name = BaseName
                  , id        = Id},
    {ok, State, MaxAge}.

% ok | {error, Reason}
handle_message({exec, Program, Args}, State) ->
    Id = State#state.id,
    Response = tentacles_hooks:execute_hook(execute, [Id, Program, Args]),
    {reply, Response, State};

% {ok, Response} | {error, Reason}
handle_message({send, Message}, State) ->
    Id = State#state.id,
    Response = tentacles_hooks:execute_hook(send, [Id, Message]),
    {reply, Response, State};

% pong | pang
handle_message(ping, State) ->
    Id = State#state.id,
    Response = tentacles_hooks:execute_hook(ping, [Id]),
    {reply, Response, State};

% ok | {error, Reason}
handle_message(die, State) ->
    Id = State#state.id,
    Response = tentacles_hooks:execute_hook(die, [Id]),
    {reply, Response, State}.

handle_timeout(State) ->
    BaseName = State#state.base_name,
    Id       = State#state.id,
    tentacles_dispatcher:expire(BaseName, Id),
    {stop, normal, State}.

handle_event(_Event, State) ->
    {noreply, State}.

handle_termination(_Reason, _State) ->
    ok.
