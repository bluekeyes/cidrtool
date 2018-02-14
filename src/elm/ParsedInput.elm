module ParsedInput
    exposing
        ( Config
        , Customizations
        , Model
        , Msg
        , Validity(..)
        , config
        , init
        , reset
        , update
        , view
        )

import Debounce exposing (Debounce)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Event
import Html.Keyed as Keyed
import Task
import Time


type Config a msg
    = Config
        { parser : String -> Result String a
        , onValue : Maybe a -> msg
        , onMsg : Msg a -> msg
        , customizations : Customizations msg
        }


config :
    { parser : String -> Result String a
    , onValue : Maybe a -> msg
    , onMsg : Msg a -> msg
    , customizations : Customizations msg
    }
    -> Config a msg
config { parser, onValue, onMsg, customizations } =
    Config
        { parser = parser
        , onValue = onValue
        , onMsg = onMsg
        , customizations = customizations
        }


type Validity
    = Valid
    | Invalid


type alias Customizations msg =
    { attrs : List (Attribute msg)
    , inputAttrs : Validity -> List (Attribute msg)
    , error : Maybe String -> Html msg
    }


type Model
    = Model
        { key : Int
        , raw : String
        , error : Maybe String
        , debounce : Debounce String
        }


type Msg a
    = Input String
    | Parsed (Result String a)
    | DebounceMsg Debounce.Msg


init : Model
init =
    Model
        { key = 0
        , raw = ""
        , error = Nothing
        , debounce = Debounce.init
        }


validity : Maybe String -> Validity
validity error =
    case error of
        Just _ ->
            Invalid

        Nothing ->
            Valid


update : Config a msg -> Msg a -> Model -> ( Model, Cmd msg )
update (Config config) msg (Model model) =
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
                    ( Model { model | error = Nothing }
                    , Task.perform config.onValue (Task.succeed (Just value))
                    )

                Err error ->
                    let
                        newError =
                            if model.raw == "" then
                                Nothing
                            else
                                Just error
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
        , error = Nothing
        , debounce = Debounce.init
        }


view : Config a msg -> Model -> Html msg
view (Config { onMsg, customizations }) (Model model) =
    let
        attrs =
            customizations.attrs

        inputAttrs =
            Attr.type_ "text"
                :: Attr.defaultValue model.raw
                :: Event.onInput (Input >> onMsg)
                :: customizations.inputAttrs (validity model.error)

        error =
            customizations.error model.error
    in
    Keyed.node "div"
        attrs
        [ ( toString model.key, Html.input inputAttrs [] )
        , ( "error", error )
        ]



--- INTERNAL HELPERS ---


debounceConfig : (Msg a -> msg) -> Debounce.Config msg
debounceConfig onMsg =
    { strategy = Debounce.later (500 * Time.millisecond)
    , transform = DebounceMsg >> onMsg
    }
