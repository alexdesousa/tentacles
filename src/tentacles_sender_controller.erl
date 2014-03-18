-module(tentacles_sender_controller).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { id      :: any()
               , max_age :: tentacles_server:seconds()
               }).

start_link(Id, MaxAge) ->
    gen_server:start_link(?MODULE, [Id, MaxAge], []).

init([Id, MaxAge]) ->
    {ok, #state{id = Id, max_age = MaxAge}, MaxAge * 1000}.

handle_call(_From, _, State) ->
    {noreply, State}.

handle_cast({{exec, Id, Program, Args, BadNodes}, From, Timestamp}, State) ->
    % Pick a node
    % Try to send to S nodes
    % if succedes -> reply successful
    % else -> reply error.
    NewSubscribers = add_subscriber(Subscriber, State#state.subscribers),
    gen_server:reply(From, {ok, now_to_micro_seconds(erlang:now())}),
    {noreply, purge_old_messages(State#state{ subscribers = NewSubscribers })};
