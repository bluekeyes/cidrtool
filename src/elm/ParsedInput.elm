module ParsedInput exposing (Config, State, initial, reset, update, view)

import Html exposing (..)
import Html.Attributes exposing (class, defaultValue, type_)
import Html.Events exposing (onInput)
import Html.Keyed


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
            { state
                | raw = raw
                , value = Nothing
                , error =
                    if raw == "" then
                        ""
                    else
                        error
            }


reset : State a -> State a
reset state =
    { key = state.key + 1
    , value = Nothing
    , raw = ""
    , error = ""
    }


type alias Config a =
    { action : String -> a
    , label : String
    , labelAttrs : List (Html.Attribute a)
    , inputAttrs : List (Html.Attribute a)
    , errorAttrs : List (Html.Attribute a)
    }


view : Config a -> List (Html.Attribute a) -> State b -> Html a
view config attrs state =
    Html.Keyed.node "div"
        attrs
        [ ( "label", label config.labelAttrs [ text config.label ] )
        , ( toString state.key
          , input
                (type_ "text"
                    :: defaultValue state.raw
                    :: onInput config.action
                    :: config.inputAttrs
                )
                []
          )
        , ( "error", span config.errorAttrs [ text state.error ] )
        ]
