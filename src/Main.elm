module Main exposing (..)

import Cidr exposing (Cidr, Ip)
import Html exposing (..)
import Html.Attributes exposing (class, defaultValue, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import ParsedInput


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias SubtractionModel =
    { subtrahends : List Cidr
    , result : List Cidr
    }


subtract : Cidr -> Cidr -> SubtractionModel -> SubtractionModel
subtract minuend subtrahend model =
    let
        subtrahends =
            subtrahend :: model.subtrahends
    in
    { model | subtrahends = subtrahends, result = Cidr.subtract minuend subtrahends }


type alias Model =
    { cidr : ParsedInput.State Cidr
    , subtrahend : ParsedInput.State Cidr
    , subtraction : SubtractionModel
    }


model : Model
model =
    { cidr = ParsedInput.initial
    , subtrahend = ParsedInput.initial
    , subtraction = SubtractionModel [] []
    }


type Msg
    = CidrInput String
    | SubtrahendInput String
    | Subtract Cidr Cidr


update : Msg -> Model -> Model
update msg model =
    case msg of
        CidrInput value ->
            let
                state =
                    ParsedInput.update Cidr.fromString value model.cidr
            in
            { model | cidr = state }

        SubtrahendInput value ->
            let
                state =
                    ParsedInput.update Cidr.fromString value model.subtrahend
            in
            { model | subtrahend = state }

        -- TODO(bkeyes): take two args or use Maybe.map2 on model?
        Subtract minuend subtrahend ->
            { model
                | subtraction = subtract minuend subtrahend model.subtraction
                , subtrahend = ParsedInput.reset model.subtrahend
            }



-- COMPONENTS --


cidrInput : (String -> Msg) -> ParsedInput.State Cidr -> Html Msg
cidrInput action state =
    Html.Keyed.node "div"
        []
        [ ( "label", label [] [ text "CIDR block" ] )
        , ( toString state.key
          , input
                [ type_ "text", placeholder "0.0.0.0/0", defaultValue state.value, onInput action ]
                []
          )
        , ( "error", small [] [ text state.error ] )
        ]


cidrInfo : Cidr -> Html Msg
cidrInfo cidr =
    div []
        [ h2 [] [ text (Cidr.toString cidr) ]
        , dl []
            [ dt [] [ text "Size" ]
            , dd [] [ text (toString (Cidr.size cidr)) ]
            , dt [] [ text "Mask" ]
            , dd [] [ text (Cidr.ipToString (Cidr.routingMask cidr)) ]
            , dt [] [ text "First Address" ]
            , dd [] [ text (Cidr.ipToString (Cidr.firstAddress cidr)) ]
            , dt [] [ text "Last Address" ]
            , dd [] [ text (Cidr.ipToString (Cidr.lastAddress cidr)) ]
            ]
        ]


tableWithRows : List String -> List (Html Msg) -> Html Msg
tableWithRows headers rows =
    table []
        [ thead [] [ tr [] (List.map (\s -> th [] [ text s ]) headers) ]
        , tbody [] rows
        ]


cidrTable : List Cidr -> Html Msg
cidrTable cidrs =
    let
        toRow : Cidr -> Html Msg
        toRow cidr =
            tr []
                [ td [] [ text (Cidr.toString cidr) ]
                , td [] [ text (Cidr.ipToString (Cidr.firstAddress cidr)) ]
                , td [] [ text (Cidr.ipToString (Cidr.lastAddress cidr)) ]
                ]
    in
    -- TODO(bkeyes): columns make it hard to copy just the blocks
    tableWithRows [ "CIDR Block", "First Address", "Last Address" ] (List.map toRow cidrs)


subtractor : Cidr -> Model -> Html Msg
subtractor minuend model =
    let
        result =
            if List.isEmpty model.subtraction.result then
                div [] [ text "No results" ]
            else
                div [] [ cidrTable model.subtraction.result ]

        submitButton =
            case model.subtrahend.value of
                Just cidr ->
                    button [ type_ "button", onClick (Subtract minuend cidr) ] [ text "Subtract" ]

                Nothing ->
                    button [ type_ "button", disabled True ] [ text "Subtract" ]
    in
    div []
        [ h2 [] [ text "Subtract" ]
        , form []
            [ cidrInput SubtrahendInput model.subtrahend
            , submitButton
            ]
        , result
        ]


view : Model -> Html Msg
view model =
    let
        input =
            cidrInput CidrInput model.cidr
    in
    case model.cidr.value of
        Just cidr ->
            div []
                [ input
                , cidrInfo cidr
                , subtractor cidr model
                ]

        Nothing ->
            div [] [ input ]
