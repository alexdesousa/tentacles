-module(tentacles_redist_dispatcher).

-behaviour(tentacles_dispatcher).

-export([start_link/1]).

% Callbacks.
-export([init/2, handle_timeout/1, handle_event/2, handle_termination/2]).

-define(REDISTRIBUTOR, tentacles_redistributor_node).

-define(MAX_LOAD, tentacles_node_max_load).
-define(DEFAULT_LOAD, 100).

-define(TRIES, 10).

-record(state, {
    stage    :: init | update | redistribute,
    priority :: integer()
}).

%-------------------------------------------------------------------------------
% Callback implementation.
%-------------------------------------------------------------------------------

start_link(Name) ->
    tentacles_dispatcher:start_link(Name, []).

init(_Name, []) ->
    MasterNode = get_redist_node(),
    if
        MasterNode =:= node() ->
            State = connect(),
            {ok, State};
        true ->
            State = connect(MasterNode, ?TRIES),
            {ok, State}
    end.

handle_timeout(State) ->
    {noreply, State}.

handle_event(_Event, State) ->
    {noreply, State}.

handle_termination(_Reason, _State) ->
    ok.
    
%-------------------------------------------------------------------------------
% Initialization functions.
%-------------------------------------------------------------------------------

-spec connect() -> State :: #state{}.
%% @doc Initializes master redistributor.
connect() ->
    connect(none, 0).

-spec connect( MasterNode :: node()
             , Tries      :: integer()) -> State :: #state{}.
%% @doc TODO: Connects to the master redistributor.
connect(_Node, 0) ->
    % Master redistributor.
    MaxLoad  = get_node_max_load(),
    case tentacles_priority:set_master_priority(MaxLoad) of
        {ok, Priority} ->
            lagger:info("Tentacles node ~s started as master.", [node()]),
            #state{
                stage    = init,
                priority = Priority
            };
        _ ->
            lagger:error("Cannot initialize tentacles node ~s.", [node()]),
            exit(cannot_set_master)
    end;
connect(MasterNode, Tries) ->
    % Normal node.
    MaxLoad = get_node_max_load(),
    case tentacles:hail(MasterNode, MaxLoad) of
        {{ok, Priority}, _} ->
            lagger:info("Tentacles node ~s started with priority ~p.", [node(), Priority]),
            #state{
                stage    = update,
                priority = Priority
            };
        _ ->
            lagger:info("Fail to hail master ~s. Tries left ~p.", [node(), Tries - 1]),
            connect(MasterNode, Tries - 1)
    end.

%-------------------------------------------------------------------------------
% Getters for environment variables.
%-------------------------------------------------------------------------------

-spec get_redist_node() -> node().
%% @doc Gets redistributor default node.
get_redist_node() ->
    case application:get_env(tentacles, ?REDISTRIBUTOR) of
        undefined ->
            node();
        {ok, Node} ->
            Node
    end.

-spec get_node_max_load() -> non_neg_integer().
%% @doc Gets node's maximum load.
get_node_max_load() ->
    case application:get_env(tentacles, ?MAX_LOAD) of
        undefined  -> ?DEFAULT_LOAD;
        {ok, Load} -> Load
    end.
