-module(tentacles_default_hooks).

-behaviour(tentacles_hooks).

% Callbacks.
-export([execute/3, send/2, ping/1, die/1, whois_broadcast/0]).

execute(_Id, _Program, _Args) -> ok.

send(_Id, Message) -> {ok, received}.

ping(_Id) -> pong.

die(Id) ->
    tentacles:suicide(Id),%% *-node should suicide.
    ok.

whois_broadcast() -> {ok, []}.
