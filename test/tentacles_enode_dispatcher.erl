-module(tentacles_enode_dispatcher).

-behaviour(tentacles_dispatcher).

-export([start_link/1]).

% Callbacks.
-export([init/2, handle_timeout/1, handle_event/2, handle_termination/2]).

start_link(Name) ->
    tentacles_dispatcher:start_link(Name, []).

init(Name, []) ->
    {ok, []}.

handle_timeout(State) ->
    {noreply, State}.

handle_event(_Event, State) ->
    {noreply, State}.

handle_termination(_Reason, _State) ->
    ok.
