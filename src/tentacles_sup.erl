-module(tentacles_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%-------------------------------------------------------------------------------
% Public functions.
%-------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%-------------------------------------------------------------------------------
% Supervisor callbacks.
%-------------------------------------------------------------------------------

init([]) ->
    %{BossDBSup, BossNewsSup} = get_database(),
    Sender   = get_server(sender, []),
    Receiver = get_server(receiver, []),
    Redist   = get_server(redist, []),

    Children = [Sender, Receiver, Redist],
    RestartStrategy = {one_for_one, 10, 10},

    {ok, {RestartStrategy, Children}}.

%-------------------------------------------------------------------------------
% Private functions.
%-------------------------------------------------------------------------------

%% @doc Generates server supervisor.
get_server(Name, Args) ->
    ServerName = list_to_atom("tentacles_" ++ atom_to_list(Name) ++ "_sup"),
    { ServerName
    , {tentacles_server_sup, start_link, [Name, Args]}
    , permanent, 2000, supervisor, [tentacles_server_sup]}.

%get_database() ->
%    DBOptions = case application:get_env(tentacles, tentacles_boss_db) of
%        undefined -> [];
%        {ok, Val} -> Val
%    end,
%    BossDBSup   = { t_boss_db
%                  , {boss_db, start, [DBOptions]}
%                  , permanent, 2000, supervisor, [boss_db]},
%    BossNewsSup = { t_boss_news
%                  , {boss_news, start, []}
%                  , permanent, 2000, supervisor, [boss_news]},
%    {BossDBSup, BossNewsSup}.
