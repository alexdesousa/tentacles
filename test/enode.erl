-module(enode).

-behaviour(tentacles_hooks).

-export([start/0]).

% Callbacks.
-export([execute/3, send/2, ping/1, die/1, whois_broadcast/0]).

start() ->
    tentacles_server_sup:start_link(enode, []).

%-------------------------------------------------------------------------------
% Callbacks.
%-------------------------------------------------------------------------------

execute(Id, Program, Args) ->
    enode_call(Id, {exec, Program, Args}).

send(Id, Message) ->
    enode_call(Id, {send, Message}).

ping(Id) ->
    enode_call(Id, ping).

die(Id) ->
    enode_call(Id, die).

whois_broadcast() ->
    case tentacles_dispatcher:whois_broadcast(enode) of
        {error, _} = Error -> Error;
        {Response, _}      -> Response
    end.

%-------------------------------------------------------------------------------
% Private functions.
%-------------------------------------------------------------------------------


%% @doc Calls a service in the enode.
enode_call(Id, Message) ->
    case tentacles_dispatcher:async_message(enode, node(), Id, Message) of
        {error, _} = Error -> Error;
        {Response, _}      -> Response
    end.
