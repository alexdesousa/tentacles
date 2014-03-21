-module(tentacles_server).

-behaviour(gen_server).

-export([start_link/2, send_to_server/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(TENTACLES_CONTROLLER_MAX_AGE, 30000).

-define(TENTACLES_TIMEOUT, 5000).

%-------------------------------------------------------------------------------
% Types definitions.
%-------------------------------------------------------------------------------

-type supervisor_name() :: atom().
%% Supervisor name.

-type base_name() :: atom().
%% Base name of the server.

-type id() :: term().
%% Controller ID.

-type handler() :: pid().
%% Pid of the controller.

-type server_ref() :: Name :: atom()
                    | {Name :: atom(), Node :: node()}
                    | {global, GlobalName :: term()}
                    | {via, Module :: atom(), ViaName :: term()}
                    | pid().
%% Server reference.

-type message() :: {async, {id(), tentacles_controller:message()}, erlang:timestamp()}
                 | {sync, {id(), tentacles_controller:message()}, erlang:timestamp()}

%% @doc Internal state of the server.
-record(state, { dict       :: dict:dict()
               , supervisor :: supervisor_name()
               , max_age    :: tentacles_controller:seconds()
               , name       :: base_name()
               }).

%-------------------------------------------------------------------------------
% Public functions.
%-------------------------------------------------------------------------------

-spec start_link(base_name()) -> {ok, pid()}
                               | ignore
                               | {error, term()}.
%% @doc Starts a tentacles server identified by `Name` and with handlers with
%% max age of `MaxAge`.
start_link(Name) ->
    % is_atom(tentacles_name_sup)
    Supervisor = list_to_atom("tentacles_" ++ atom_to_list(Name) ++ "_sup"),
    % is_atom(tentacles_name)
    ServerName = list_to_atom("tentacles_" ++ atom_to_list(Name)),
    Args = [ServerName, Supervisor],
    case gen_server:start_link( {local, ServerName}, ?MODULE, Args, []) of
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Else ->
            Else
    end.

%-------------------------------------------------------------------------------
% gen_server callbacks definitions.
%-------------------------------------------------------------------------------

-spec init(Arguments :: any()) -> {ok, State :: #state{}}.
%% @doc Initializes server state.
init([ServerName, Supervisor]) ->
    {ok, #state{ dict       = dict:new()
               , supervisor = Supervisor
               , name       = ServerName}}.

% Concurrent requests.
handle_call({async, {Id, Msg}, Timestamp}, From, State) ->
    case on_time(Timestamp) of
        true ->
            {Handler, NewState} = find_or_create_handler(Id, State),
            tentacles_controller:send_async(Handler, From, Msg),
            {noreply, NewState};
        false ->
            {noreply, State}
    end;

% Serial requests.
handle_call({sync, {Id, Msg}, Timestamp}, From, State) ->
    case on_time(Timestamp) of
        true ->
            {Handler, NewState} = find_or_create_handler(Id, State),
            Timeout  = get_connection_timeout(),
            Response = tentacles_controller:send_sync(Handler, Msg, Timeout),
            {reply, Response, NewState};
        false ->
            {noreply, NewState}
    end;

% Other requests.
handle_call(_Msg, _From, State) ->
    {noreply, State}.

% Expiration.
handle_cast({expire, Id}, State) ->
    NewState = State#state{
        dict = dict:erase(Id, State#state.dict)},
    {noreply, NewState};

% Other requests.
handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec inform_expiration(Parent, Id) ->
    gen_server:cast(Parent, {expire, Id}).

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

-spec expire(Parent :: atom(), Id :: any(), State :: any()) -> {stop, normal, State}.
%% @doc Sends a message of expiration to the server.
expire(Parent, Id, State) ->
    gen_server:cast(Parent, {expire, Id}),
    {stop, normal, State}.

%-------------------------------------------------------------------------------
% Internal functions.
%-------------------------------------------------------------------------------

-spec send_to_server(server_ref(), message()) -> tentacles_controller:response().
                    , Msg :: any()) -> Reply :: any().
% @doc Function sends message to a server.
send_to_server(ServerRef, Msg) ->
    Timeout = get_connection_timeout(),
    try
        gen_server:call(ServerRef, Msg, Timeout)
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{noproc, _}  -> {error, unavailable};
        _:_               -> {error, unknown}
    end.

-spec request_time(erlang:timestamp()) -> tentacles_controller:millisecs().
% @doc Calculates the request time given a time stamp.
request_time(Timestamp) ->
    timer:now_diff(erlang:now(), Timestamp).

-spec on_time(erlang:timestamp()) -> true | false.
%% @doc If the `Timestamp` is still on time.
on_time(Timestamp) ->
    Timeout = get_connection_timeout(),
    Ms      = request_time(Timestamp),
    if
        Ms >= Timeout ->
            false;
        true ->
            true
    end.

-spec find_or_create_handler(id(), #state{}) -> {handler(), #state{}}.
%% @doc Find or create handler to send requests for the program identified
%%      by `Id`.
find_or_create_handler(Id, #state{ dict       = Id2Handler
                                 , supervisor = Supervisor
                                 , name       = ServerName} = State) ->
    case dict:find(Id, Id2Handler) of
        {ok, Handler} ->
            {Handler, State};
        _ ->
            MaxAge = get_controller_max_age(),
            Args   = [Id, ServerName],
            {ok, Handler} = supervisor:start_child(Supervisor, Args),
            {Handler, State#state{
                        dict = dict:store(Id, Handler, Id2Handler)
                      }}
    end.

%-------------------------------------------------------------------------------
% Configuration functions (TODO:maybe retrieve data from database?).
%-------------------------------------------------------------------------------

-spec get_controller_max_age() -> tentacles_controller:millisecs().
%% @doc Gets controller max age.
get_controller_max_age() ->
    case application:get_env(tentacles, tentacles_handler_max_age) of
        undefined    -> ?TENTACLES_CONTROLLER_MAX_AGE;
        {ok, MaxAge} -> MaxAge
    end.

-spec get_connection_timeout() -> tentacles_controller:millisecs().
%% @doc Gets connection timeout time.
get_connection_timeout() ->
    case application:get_env(tentacles, tentacles_connection_timeout) of
        undefined     -> ?TENTACLES_TIMEOUT;
        {ok, Timeout} -> Timeout
    end.
