-module(tentacles_server).

-behaviour(gen_server).

-export([start_link/2, send_to_server/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TENTACLES_TIMEOUT, 5000).

-type seconds()   :: non_neg_integer().
-type millisecs() :: non_neg_integer().

-record(state, { dict       :: dict()
               , supervisor :: atom()
               , max_age    :: seconds()
               , name       :: atom()
               }).

-spec start_link( Name   :: atom()
                , MaxAge :: seconds()) -> {ok, Pid :: pid()}
                                        | ignore
                                        | {error, Error :: any()}.
%% @doc Starts a tentacles server identified by `Name` and with handlers with
%% max age of `MaxAge`.
start_link(Name, MaxAge) ->
    Supervisor = list_to_atom(atom_to_list(Name) ++ "_sup"),
    gen_server:start_link({local, Name}, ?MODULE, [Name, Supervisor, MaxAge], []).

-spec init(Arguments :: any()) -> {ok, State :: #state{}}.
%% @doc Initializes server state.
init([Name, Supervisor, MaxAge]) ->
    {ok, #state{ dict       = dict:new()
               , supervisor = Supervisor
               , max_age    = MaxAge
               , name       = Name}}.

handle_call({local, {Id, Msg}}, From, State) ->
    {Handler, NewState} = find_or_create_handler(Id, State),
    gen_server:cast(Handler, {Msg, From}),
    {noreply, NewState};
handle_call({remote, {Id, Msg}, Timestamp}, From, State) ->
    case on_time(Timestamp) of
        true ->
            {Handler, NewState} = find_or_create_handler(Id, State),
            gen_server:cast(Handler, {Msg, From}),
            {noreply, NewState};
        false ->
            {noreply, State}
    end.

handle_cast({expire, Id}, State) ->
    NewState = State#state{
        dict = dict:erase(Id, State#state.dict)},
    {noreply, NewState};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec send_to_server( Node :: local | node()
                    , Name :: atom()
                    , Msg  :: {any(), any()}) -> {Result :: any(), Time :: millisecs()}
                                               | {error, Reason :: any()}.
%% @doc Sends message `Msg` to the server `Name` in the specified `Node`.
send_to_server(Node, Name, {_, _} = Msg) ->
    Timestamp = erlang:now(),
    Reply = case Node of
        local ->
            send_to_server({Name, node()}, {local, Msg});
        _     ->
            send_to_server({Name, Node}, {remote, Msg, Timestamp})
    end,
    {Reply, request_time(Timestamp)}.

-spec send_to_server( Name :: atom()
                    | {Name :: atom(), Node :: atom()}
                    , Msg :: any()) -> Reply :: any().
% @doc Function sends message to a server.
send_to_server(ServerRef, Msg) ->
    Timeout = get_timeout(),
    try
        gen_server:call(ServerRef, Msg, Timeout)
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{noproc, _}  -> {error, unavailable};
        _:_               -> {error, unknown}
    end.

-spec request_time(Timestamp :: erlang:timestamp()) -> millisecs().
% @doc Calculates the request time given a time stamp.
request_time(Timestamp) ->
    timer:now_diff(erlang:now(), Timestamp).

-spec on_time(Timestamp :: erlang:timestamp()) -> true | false.
%% @doc If the `Timestamp` is still on time.
on_time(Timestamp) ->
    Timeout = get_timeout(),
    Ms = timer:now_diff(erlang:now(), Timestamp),
    if
        Ms >= Timeout ->
            false;
        true ->
            true
    end.

% internal

-spec find_or_create_handler( Id         :: any()
                            , State      :: #state{}) -> { Handler :: pid()
                                                         , State   :: #state{}}.
%% @doc Find or create handler to send requests for the program identified
%% by `Id`.
find_or_create_handler(Id, #state{ dict       = Id2Handler
                                 , supervisor = Supervisor
                                 , max_age    = MaxAge
                                 , name       = Name} = State) ->
    case dict:find(Id, Id2Handler) of
        {ok, Handler} ->
            {Handler, State};
        _ ->
            {ok, Handler} = supervisor:start_child(Supervisor, [Id, Name, MaxAge]),
            {Handler, State#state{
                        dict = dict:store(Id, Handler, Id2Handler)
                      }}
    end.

-spec get_timeout() -> millisecs().
%% @doc Gets connection timeout time.
get_timeout() ->
    case application:get_env(tentacles, tentacles_connection_timeout) of
        undefined -> ?TENTACLES_TIMEOUT;
        {ok, T}   -> T
    end.
            
