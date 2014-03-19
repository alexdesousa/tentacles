-module(tentacles_program, [ Id
                           , Hash
                           , Location
                           , Priority
                           , State
                           , Name
                           , Args]).

validation_tests() ->
    [ {fun() -> is_atom(Location) end, "Bad node."}
    , {fun() -> check_state(State) end, "Program state is invalid."}
    , {fun() -> is_atom(Name) end, "Bad program name."}
    ].

check_state({normal, running}) -> true;
check_state({normal, pending}) -> true;
check_state({normal, down})    -> true;
check_state({normal, kill})    -> true;
check_state({emergency, running}) -> true;
check_state({emergency, pending}) -> true;
check_state({emergency, down})    -> true;
check_state({emergency, kill})    -> true;
check_state(_Else) -> false.
