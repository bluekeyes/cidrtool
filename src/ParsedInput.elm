module ParsedInput exposing (State, initial, reset, update)


type alias State a =
    { key : Int
    , value : Maybe a
    , raw : String
    , error : String
    }


initial : State a
initial =
    { key = 0
    , value = Nothing
    , raw = ""
    , error = ""
    }


update : (String -> Result String a) -> String -> State a -> State a
update parser raw state =
    case parser raw of
        Ok value ->
            { state | raw = raw, value = Just value, error = "" }

        Err error ->
            { state | raw = raw, value = Nothing, error = error }


reset : State a -> State a
reset state =
    { key = state.key + 1
    , value = Nothing
    , raw = ""
    , error = ""
    }
