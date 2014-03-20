-module(tentacles_server_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Name) ->
    Supervisor = list_to_atom(atom_to_list(Name) ++ "_sup"),
    supervisor:start_link({local, Supervisor}, ?MODULE, [Name]).

init([Name]) ->
    WorkerName   = list_to_atom(atom_to_list(Name) ++ "_handler"),
    WorkerModule = list_to_atom(atom_to_list(Name) ++ "_controller"),
    {ok, {{simple_one_for_one, 0, 10},
          [
           {WorkerName, {WorkerModule, start_link, []},
            temporary, 2000, worker, [WorkerModule]}
          ]}}.
