module ParsedInput exposing (Config, Model, Msg, ViewConfig, initial, reset, update, view)

import Debounce exposing (Debounce)
import Html exposing (..)
import Html.Attributes exposing (class, defaultValue, type_)
import Html.Events exposing (onInput)
import Html.Keyed
import Task
import Time


type alias Config a msg =
    { parser : String -> Result String a
    , onValue : Maybe a -> msg
    , onMsg : Msg a -> msg
    , onView : Bool -> ViewConfig msg
    }


type alias ViewConfig msg =
    { label : String
    , labelAttrs : List (Html.Attribute msg)
    , inputAttrs : List (Html.Attribute msg)
    , errorAttrs : List (Html.Attribute msg)
    }


type Model
    = Model
        { key : Int
        , raw : String
        , error : String
        , debounce : Debounce String
        }


type Msg a
    = Input String
    | Parsed (Result String a)
    | DebounceMsg Debounce.Msg


initial : Model
initial =
    Model
        { key = 0
        , raw = ""
        , error = ""
        , debounce = Debounce.init
        }


update : Config a msg -> Msg a -> Model -> ( Model, Cmd msg )
update config msg (Model model) =
    case msg of
        Input raw ->
            let
                ( debounce, cmd ) =
                    Debounce.push (debounceConfig config.onMsg) raw model.debounce
            in
            ( Model { model | raw = raw, debounce = debounce }, cmd )

        Parsed result ->
            case result of
                Ok value ->
                    ( Model { model | error = "" }
                    , Task.perform config.onValue (Task.succeed (Just value))
                    )

                Err error ->
                    let
                        newError =
                            if model.raw == "" then
                                ""
                            else
                                error
                    in
                    ( Model { model | error = newError }
                    , Task.perform config.onValue (Task.succeed Nothing)
                    )

        DebounceMsg msg ->
            let
                parse raw =
                    config.parser raw
                        |> Parsed
                        |> Task.succeed
                        |> Task.perform config.onMsg

                ( debounce, cmd ) =
                    Debounce.update
                        (debounceConfig config.onMsg)
                        (Debounce.takeLast parse)
                        msg
                        model.debounce
            in
            ( Model { model | debounce = debounce }, cmd )


reset : Model -> Model
reset (Model model) =
    Model
        { key = model.key + 1
        , raw = ""
        , error = ""
        , debounce = Debounce.init
        }


view : Config a msg -> List (Html.Attribute msg) -> Model -> Html msg
view config attrs (Model model) =
    let
        viewConfig =
            config.onView (model.error /= "")
    in
    Html.Keyed.node "div"
        attrs
        [ ( "label", label viewConfig.labelAttrs [ text viewConfig.label ] )
        , ( toString model.key
          , input
                (type_ "text"
                    :: defaultValue model.raw
                    :: onInput (Input >> config.onMsg)
                    :: viewConfig.inputAttrs
                )
                []
          )
        , ( "error", span viewConfig.errorAttrs [ text model.error ] )
        ]



--- INTERNAL HELPERS ---


debounceConfig : (Msg a -> msg) -> Debounce.Config msg
debounceConfig onMsg =
    { strategy = Debounce.later (500 * Time.millisecond)
    , transform = DebounceMsg >> onMsg
    }
