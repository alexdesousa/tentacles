-module(tentacles_program, [ Id
                           , RealId
                           , Location
                           , Priority :: integer()
                           , State
                           , Name
                           , Args]).

validation_tests() ->
    [ {fun() -> is_atom(Location) end, "Bad node."}
    , {fun() -> is_atom(Name) end, "Bad program name."}
    ].
