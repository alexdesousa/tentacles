-module(tentacles_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(DEFAULT_HANDLER_MAX_AGE, 30).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {BossDBSup, BossNewsSup} = get_database(),
    {Sender, SenderSup}      = get_server(sender),
    {Receiver, ReceiverSup}  = get_server(receiver),

    Children = [BossDBSup, BossNewsSup, Sender, Receiver, SenderSup, ReceiverSup],
    RestartStrategy = {one_for_one, 10, 10},

    {ok, {RestartStrategy, Children}}.

get_database() ->
    DBOptions = case application:get_env(tentacles, tentacles_boss_db) of
        undefined -> [];
        {ok, Val} -> Val
    end,
    BossDBSup   = { t_boss_db
                  , {boss_db, start, [DBOptions]}
                  , permanent, 2000, supervisor, [boss_db]},
    BossNewsSup = { t_boss_news
                  , {boss_news, start, []}
                  , permanent, 2000, supervisor, [boss_news]},
    {BossDBSup, BossNewsSup}.

get_server(Name) ->
    ServerName         = list_to_atom("t_" ++ atom_to_list(Name) ++ "_controller"),
    Server = { ServerName
             , {tentacles_server, start_link, [Name]}
             , permanent, 2000, worker, [tentacles_server]},
    
    SupervisorName = list_to_atom("t_" ++ atom_to_list(Name) ++ "_sup"),
    SenderSup = { SupervisorName
                , {tentacles_server_sup, start_link, [Name]}
                , permanent, 2000, supervisor, [tentacles_server_sup]},
    {Server, SenderSup}.
