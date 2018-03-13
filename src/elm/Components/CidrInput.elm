module Components.CidrInput
    exposing
        ( Model
        , Msg
        , Style(..)
        , getValue
        , init
        , reset
        , update
        , view
        )

import Cidr exposing (Cidr)
import Debounce exposing (Debounce)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Event
import Html.Keyed as Keyed
import Task
import Time


type Style
    = Normal
    | Large


type alias ModelData =
    { raw : String
    , value : Maybe Cidr
    , error : Maybe String
    , debounce : Debounce String
    , inputKey : Int
    }


type Model
    = Model ModelData


type Msg
    = Input String
    | Parsed (Result String Cidr)
    | DebounceMsg Debounce.Msg


getValue : Model -> Maybe Cidr
getValue (Model { value }) =
    value


init : Model
init =
    Model
        { raw = ""
        , value = Nothing
        , error = Nothing
        , debounce = Debounce.init
        , inputKey = 0
        }


reset : Model -> Model
reset (Model data) =
    Model
        { raw = ""
        , value = Nothing
        , error = Nothing
        , debounce = Debounce.init
        , inputKey = data.inputKey + 1
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data) =
    case msg of
        Input raw ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig raw data.debounce
            in
            ( Model { data | raw = raw, debounce = debounce }, cmd )

        Parsed result ->
            case result of
                Ok value ->
                    ( Model { data | value = Just value, error = Nothing }, Cmd.none )

                Err error ->
                    ( Model (updateError data error), Cmd.none )

        DebounceMsg msg ->
            let
                parse raw =
                    Cidr.fromString raw
                        |> Task.succeed
                        |> Task.perform Parsed

                ( debounce, cmd ) =
                    Debounce.update debounceConfig (Debounce.takeLast parse) msg data.debounce
            in
            ( Model { data | debounce = debounce }, cmd )


view : Style -> List (Attribute Msg) -> Model -> Html Msg
view style attrs (Model data) =
    Keyed.node "div"
        attrs
        [ ( toString data.inputKey, viewInput style data )
        , ( "error"
          , case data.error of
                Nothing ->
                    Html.text ""

                Just msg ->
                    viewError style msg
          )
        ]


viewInput : Style -> ModelData -> Html Msg
viewInput style data =
    let
        hasError =
            data.error /= Nothing

        classlist =
            -- TODO(bkeyes): move most of this into the CSS file
            Attr.classList
                [ ( "block w-full py-2 appearance-none text-center rounded-sm bg-white border-gray-light", True )
                , ( "text-lg border", style == Normal )
                , ( "no-outline text-4xl border-b-8", style == Large )
                , ( "text-gray-darkest", not hasError )
                , ( "text-red-dark", hasError )
                , ( "hover:border-blue focus:border-blue", style == Large && not hasError )
                , ( "hover:border-red-dark focus:border-red-dark", style == Large && hasError )
                ]

        attrs =
            [ Attr.placeholder "0.0.0.0/0"
            , Attr.type_ "text"
            , Attr.defaultValue data.raw
            , Event.onInput Input
            , classlist
            ]
    in
    Html.input attrs []


viewError : Style -> String -> Html Msg
viewError style error =
    let
        classlist =
            -- TODO(bkeyes): move most of this into the CSS file
            -- TODO(bkeyes): can both styles use the same positioning?
            Attr.classList
                [ ( "arrow-tc arrow-red-dark rounded shadow-md text-sm text-white text-center bg-red-dark", True )
                , ( "absolute pin-x pin-u p-2 mt-3 arrow-2", style == Normal )
                , ( "relative p-2 mt-4 arrow-3", style == Large )
                ]
    in
    Html.div [ classlist ] [ Html.text error ]


updateError : ModelData -> String -> ModelData
updateError data error =
    if data.raw == "" then
        { data | error = Nothing }
    else
        { data | value = Nothing, error = Just error }


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later (250 * Time.millisecond)
    , transform = DebounceMsg
    }