-module(tentacles_priority).

-export([set_master_priority/1, set_priority/2]).

-spec set_master_priority(MaxLoad :: integer()) -> {ok, Priority :: integer()}
                                                 | {error, term()}.
%% @doc Sets master's priority.
set_master_priority(MaxLoad) ->
    reset_old_master(),
    NewRecord = case boss_db:find_first(tentacles_node, [{name, 'equals', node()}]) of
        undefined ->
            tentacles_node:new(id, node(), 0, MaxLoad, 0, up, erlang:now());
        Record    ->
            Record:set([
                {priority, 0},
                {max_load, MaxLoad},
                {state, up},
                {timestamp, erlang:now()}
            ])
    end,
    case NewRecord:save() of
        {ok, SavedRecord} ->
            {ok, SavedRecord:priority()};
        Errors ->
            Errors
    end.

-spec reset_old_master() -> {ok, Record :: term()} | {error, term()}.
%% @doc Resets old master.
reset_old_master() ->
    case boss_db:find_first(tentacles_node, [{priority, 'equals', 0}]) of
        undefined ->
            ok;
        Record    ->
            MaxLoad = Record:max_load(),
            Record:set([
                {priority, MaxLoad},
                {state, redistribute},
                {timestamp, erlang:now()}
            ]),
            Record:save()
    end.

-spec set_priority( Node    :: node()
                  , MaxLoad :: integer()) -> {ok, Priority :: integer()}
                                           | {error, term()}.
%% @doc Sets node priority.
set_priority(_Node, 0) ->
    {error, bad_max_load};
set_priority(Node, MaxLoad) ->
    case update_priority(Node, MaxLoad) of
        {ok, SavedRecord} ->
            {ok, SavedRecord:priority()};
        Errors ->
            Errors
    end.

-spec update_priority( Node    :: node()
                     , MaxLoad :: integer()) -> {ok, term()} | {error, [term()]}.
%% @doc Updates node in the database.
update_priority(Node, MaxLoad) ->
    case boss_db:find_first(tentacles_node, [{name, 'equals', Node}]) of
        undefined ->
            update_priority(new, Node, MaxLoad);
        Record    ->
            update_priority(Record, Node, MaxLoad)
    end.

-spec update_priority( new
                     , Node     :: node()
                     , MaxLoad  :: integer())    ->
                        {ok, term()} | {error, [term()]};
                     ( BossRecord :: term()
                     , Node       :: node()
                     , MaxLoad    :: integer()) ->
                        {ok, term()} | {error, [term()]}.
%% @doc Updates node in the database.
update_priority(new, Node, MaxLoad)    ->
    Record   = tentacles_node:new(id, Node, MaxLoad, MaxLoad, 0, up, erlang:now()),
    Record:save();
update_priority(Record, _Node, MaxLoad) ->
    Priority = case Record:max_load() of
        MaxLoad ->
            Record:priority() + 1;
        _ ->
            MaxLoad
    end,
    NewRecord = Record:set([{priority, Priority}, {state, redistribute}]),
    NewRecord:save().
