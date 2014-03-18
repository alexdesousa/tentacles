-module(tentacles_server).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type seconds() :: non_neg_integer()

-record(state, { dict       :: dict()
               , supervisor :: atom()
               , max_age    :: seconds()
               }).

-spec start_link( Name   :: atom()
                , MaxAge :: seconds()) -> {ok, Pid :: pid()}
                                        | ignore
                                        | {error, Error}.
%% @doc Starts a tentacles server identified by `Name` and with handlers with
%% max age of `MaxAge`.
start_link(Name, MaxAge) ->
    Supervisor = list_to_atom(atom_to_list(Name) ++ "_sup"),
    gen_server:start_link({local, Name}, ?MODULE, [Name, Supervisor, MaxAge], []).


%    RiakServer     = proplists:get_value(tentacles_riak_server, Config, ?DEFAULT_RIAK_SERVER),
%    RiakPort       = proplists:get_value(tentacles_riak_port, Config, ?DEFAULT_RIAK_PORT),
%    RiakNVal       = proplists:get_value(tentacles_riak_n_val, Config, ?DEFAULT_RIAK_N_VAL),
%    RiakRVal       = proplists:get_value(tentacles_riak_r_val, Config, ?DEFAULT_RIAK_R_VAL),
%    RiakWVal       = proplists:get_value(tentacles_riak_w_val, Config, ?DEFAULT_RIAK_W_VAL),

-spec init(Arguments :: any()) -> {ok, State :: #state{}}.
%% @doc Initializes server state.
init([Name, Supervisor, MaxAge]) ->
    {ok, #state{ dict       = dict:new()
               , supervisor = Supervisor
               , max_age    = MaxAge}}.

handle_call(Msg, From, State) ->
    {Handler, NewState} = find_or_create_handler(Id, State),
    gen_server:cast(Handler, {Msg, From, erlang:now()}),
    {noreply, NewState}.

handle_cast({set_max_age, NewMaxAge}, State) ->
    {noreply, State#state{max_age = NewMaxAge}};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

% internal

-spec find_or_create_handler( Id         :: any()
                            , State      :: #state{}) -> { Handler :: pid()
                                                         , State   :: #state{}}.
%% @doc Find or create handler to send requests for the program identified
%% by `Id`.
find_or_create_handler(Id, #state{ dict       = Id2Handler
                                 , supervisor = Supervisor
                                 , max_age    = MaxAge} = State) ->
    case dict:find(Id, Id2Handler) of
        {ok, Handler} ->
            {Handler, State};
        _ ->
            {ok, Handler} = supervisor:start_child(Supervisor, [Id, MaxAge]),
            {Handler, State#state{
                        dict = dict:store(Id, Handler, Id2Handler)
                      }}
    end.
