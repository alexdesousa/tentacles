-module(tentacles_node, [ Id
                        , Name
                        , MaxLoad  :: integer()
                        , Priority :: integer()
                        , Load     :: integer()
                        , State
                        , Update   :: timestamp()]).

validation_tests() ->
    [ {fun() -> is_atom(Name) end, "Bad node."}
    , {fun() -> Load =< MaxLoad end, "Load is invalid."}
    , {fun() -> check_state(State) end, "Node state is invalid."}
    ].

check_state(up)    -> true;
check_state(down)  -> true;
check_state(_Else) -> false.
