-module(tentacles_enode_controller).

-behaviour(tentacles_controller).

% Callbacks.
-export([init/2, handle_message/2, handle_timeout/1, handle_event/2,
         handle_termination/2]).

-record(state, { base_name         :: tentacles_dispatcher:base_name()
               , id                :: tentacles_dispatcher:id()
               , executing = false :: true | false
               , pid = none        :: none | pid()}).

init(BaseName, Id) ->
    State = #state{ base_name = BaseName
                  , id        = Id},
    {ok, State}.

% execute hook.
handle_message({exec, _Program, _Args}, State) ->
    case State#state.executing of
        false ->
            %Execute.
            NewState = State#state{executing = true, pid = none},
            {reply, ok, NewState};
        true ->
            {reply, ok, State}
    end;

% send hook.
handle_message({send, _Message}, State) ->
    case State#state.executing of
        true ->
            %Send to process and get response.
            {reply, {ok, received}, State};
        false ->
            {stop, normal, {error, no_such_process}, State}
    end;

% ping hook.
handle_message(ping, State) ->
    case State#state.executing of
        true ->
            %Send to process and get response.
            {reply, pong, State};
        false ->
            {stop, normal, pang, State}
    end;

handle_message(die, State) ->
    case State#state.executing of
        true ->
            % Send die signal to node. It should send a suicide signal to
            % tentacles and respond accordingly.
            case tentacles:suicide(State#state.id) of
                {ok, _} ->
                    {stop, normal, ok, State};
                Error   ->
                    {reply, Error, State}
            end;
        false ->
            {stop, normal, ok, State}
    end;

% Any.
handle_message(_Any, State) ->
    {noreply, State}.

handle_timeout(State) ->
    {noreply, State}.

handle_event(_Event, State) ->
    {noreply, State}.

handle_termination(_Reason, _State) ->
    ok.
