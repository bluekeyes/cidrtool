module Main exposing (..)

import Cidr exposing (Cidr, Ip)
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { cidr : Cidr
    , parseError : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Cidr.zero "", Cmd.none )


type Msg
    = NewInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewInput input ->
            case Cidr.fromString input of
                Ok cidr ->
                    ( { model | cidr = cidr, parseError = "" }, Cmd.none )

                Err err ->
                    ( { model | cidr = Cidr.zero, parseError = err }, Cmd.none )


test : Cidr -> Html Msg
test cidr =
    let
        cidrToRow : Cidr -> Html Msg
        cidrToRow cidr =
            tr []
                [ td [] [ text (Cidr.toString cidr) ]
                , td [] [ text (Cidr.ipToString (Cidr.lastAddress cidr)) ]
                ]

        makeTable : List String -> List (Html Msg) -> Html Msg
        makeTable headers rows =
            table []
                [ thead [] [ tr [] (List.map (\s -> th [] [ text s ]) headers) ]
                , tbody [] rows
                ]
    in
    Cidr.fromString "10.0.0.0/8"
        |> Result.map (\c -> Cidr.subtract c [ cidr ])
        |> Result.map (\cs -> makeTable [ "CIDR", "Last IP" ] (List.map cidrToRow cs))
        |> Result.withDefault (div [] [ text "invalid cidr literal in code" ])


cidrInput : (String -> Msg) -> String -> Html Msg
cidrInput action error =
    div [ class "measure pa3 bg-light-gray ba b--moon-gray br1 ma3" ]
        [ label [ class "f6 b db mb2" ] [ text "CIDR block" ]
        , input [ class "input-reset ba b--black-20 pa2 mb2 db w-100", type_ "text", placeholder "0.0.0.0/0", onInput action ] []
        , small [ class "f6 red db mb2" ] [ text error ]
        ]


cidrInfo : Cidr -> Html Msg
cidrInfo cidr =
    div []
        [ h2 [] [ text (Cidr.toString cidr) ]
        , dl []
            [ dt [] [ text "Size" ]
            , dd [] [ text (toString (Cidr.size cidr)) ]
            , dt [] [ text "First Address" ]
            , dd [] [ text (Cidr.ipToString (Cidr.firstAddress cidr)) ]
            , dt [] [ text "Last Address" ]
            , dd [] [ text (Cidr.ipToString (Cidr.lastAddress cidr)) ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ cidrInput NewInput model.parseError
        , cidrInfo model.cidr
        , test model.cidr
        ]
