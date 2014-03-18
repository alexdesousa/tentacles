-module(tentacles_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    tentacles_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).

simple_test() ->
    ok = application:start(tentacles),
    ?assertNot(undefined == whereis(tentacles_sup)).
    
-endif.

%-define(DEFAULT_NLL, 100).
%-define(DEFAULT_ELL, 200).
%-define(DEFAULT_RIAK_SERVER, "127.0.0.1").
%-define(DEFAULT_RIAK_PORT, 10017).
%-define(DEFAULT_RIAK_N_VAL, 3).
%-define(DEFAULT_RIAK_R_VAL, 3).
%-define(DEFAULT_RIAK_W_VAL, 3).

%start(Config) ->
%    NLL            = proplists:get_value(tentacles_nll, Config, ?DEFAULT_NLL),
%    ELL            = proplists:get_value(tentacles_ell, Config, ?DEFAULT_ELL),
%    RiakServer     = proplists:get_value(tentacles_riak_server, Config, ?DEFAULT_RIAK_SERVER),
%    RiakPort       = proplists:get_value(tentacles_riak_port, Config, ?DEFAULT_RIAK_PORT),
%    RiakNVal       = proplists:get_value(tentacles_riak_n_val, Config, ?DEFAULT_RIAK_N_VAL),
%    RiakRVal       = proplists:get_value(tentacles_riak_r_val, Config, ?DEFAULT_RIAK_R_VAL),
%    RiakWVal       = proplists:get_value(tentacles_riak_w_val, Config, ?DEFAULT_RIAK_W_VAL),
%    {ok, RiakConn} = riakc_pb_socket:start_link(RiakServer, RiakPort),
%    gen_server:start({local, ?MODULE}, ?MODULE, [], []).
