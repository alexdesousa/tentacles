-module(tentacles_redist_controller).

-behaviour(tentacles_controller).

% Callbacks.
-export([init/2, handle_message/2, handle_timeout/1, handle_event/2,
         handle_termination/2]).

-record(state, { base_name :: tentacles_dispatcher:base_name()
               , node      :: tentacles_dispatcher:id()}).

init(BaseName, Id) ->
    State = #state{ base_name = BaseName
                  , node      = Id},
    {ok, State}.

handle_message({hail, _MaxLoad}, State) ->
    %TODO: Calculate Priority.
    %TODO: Add to database.
    {reply, {ok, 42}, State};
handle_message(dismiss, State) ->
    %TODO: Erase from database. 
    {reply, ok, State};
handle_message(_Any, State) ->
    {noreply, State}.

handle_timeout(State) ->
    {noreply, State}.

handle_event(_Event, State) ->
    {noreply, State}.

handle_termination(_Reason, _State) ->
    ok.
