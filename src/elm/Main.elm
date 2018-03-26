module Main exposing (..)

import Cidr exposing (Cidr, Ip)
import Components.CidrInput as CidrInput
import Components.Subtractor as Subtractor
import Html exposing (Html, dd, div, dl, dt, h1, h2, header, label, table, text)
import Html.Attributes exposing (class)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { cidr : CidrInput.Model
    , subtractor : Subtractor.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { cidr = CidrInput.init
      , subtractor = Subtractor.init
      }
    , Cmd.none
    )


type Msg
    = CidrMsg CidrInput.Msg
    | SubtractMsg Subtractor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CidrMsg msg ->
            let
                ( cidr, cmd ) =
                    CidrInput.update msg model.cidr
            in
            ( { model | cidr = cidr }, Cmd.map CidrMsg cmd )

        SubtractMsg msg ->
            let
                ( subtractor, cmd ) =
                    Subtractor.update msg model.subtractor
            in
            ( { model | subtractor = subtractor }, Cmd.map SubtractMsg cmd )



-- COMPONENTS --


cidrInfo : Cidr -> Html Msg
cidrInfo cidr =
    div [ class "mb-4 p-4 bg-white shadow rounded-sm w-full" ]
        [ h2 [ class "pb-4 mb-4 text-2xlg text-center leading-none border-b border-gray-light" ] [ text "Details" ]
        , dl [ class "dl-grid dl-grid-center mx-auto text-lg" ]
            [ dt [ class "font-bold" ] [ text "Size" ]
            , dd [] [ text (toString (Cidr.size cidr)) ]
            , dt [ class "font-bold" ] [ text "Mask" ]
            , dd [] [ text (Cidr.ipToString (Cidr.routingMask cidr)) ]
            , dt [ class "font-bold" ] [ text "First Address" ]
            , dd [] [ text (Cidr.ipToString (Cidr.firstAddress cidr)) ]
            , dt [ class "font-bold" ] [ text "Last Address" ]
            , dd [] [ text (Cidr.ipToString (Cidr.lastAddress cidr)) ]
            ]
        ]


appHeader : Model -> Html Msg
appHeader model =
    header [ class "w-full my-4 flex flex-col items-center text-black" ]
        [ h1
            [ class "text-5xl mb-6" ]
            [ text "cidrtool" ]
        , div
            [ class "flex flex-col items-center w-full" ]
            [ label
                [ class "text-l mb-2" ]
                [ text "Enter a CIDR block:" ]
            , cidrInput model
            ]
        ]


cidrInput : Model -> Html Msg
cidrInput model =
    CidrInput.view
        { style = CidrInput.Large, onInput = CidrMsg }
        [ class "w-full" ]
        model.cidr


view : Model -> Html Msg
view model =
    let
        children =
            case CidrInput.getValue model.cidr of
                Just cidr ->
                    [ appHeader model
                    , cidrInfo cidr
                    , Subtractor.view SubtractMsg cidr model.subtractor
                    ]

                Nothing ->
                    [ appHeader model ]
    in
    div [ class "max-w-sm mx-auto flex flex-col items-center" ] children
