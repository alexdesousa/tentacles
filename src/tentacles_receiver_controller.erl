-module(tentacles_receiver_controller).

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

% pong | pang.
handle_cast({ping, From}, State) ->
    gen_server:reply(From, pong),
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
