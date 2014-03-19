-module(tentacles_node, [ Id
                        , Name
                        , Priority :: integer()
                        , Nll      :: integer()
                        , Ell      :: integer()
                        , Load     :: integer()
                        , State
                        , Update   :: timestamp()
                        , Requests :: integer()]).

validation_tests() ->
    [ {fun() -> is_atom(Name) end, "Bad node."}
    , {fun() -> check_load_level(Nll, Ell, Load) end, "Load is invalid."}
    , {fun() -> check_state(State) end, "Node state is invalid."}
    , {fun() -> Requests >= 0 end, "Bad request value."}
    ].

check_load_level(N, E, L) -> (N >= 0) and (E >= 0) and (N =< E) and (L =< E).

check_state(up)    -> true;
check_state(down)  -> true;
check_state(_Else) -> false.
