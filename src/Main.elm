module Main exposing (..)

import Cidr exposing (Cidr, Ip)
import Html exposing (..)
import Html.Attributes exposing (class, defaultValue, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Html.Keyed


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias InputState =
    { key : Int
    , error : String
    , value : String
    }


updateInput : (String -> Result String a) -> String -> InputState -> ( Maybe a, InputState )
updateInput parser value state =
    case parser value of
        Ok parsed ->
            ( Just parsed, { state | value = value, error = "" } )

        Err error ->
            ( Nothing, { state | value = value, error = error } )


resetInput : InputState -> InputState
resetInput state =
    InputState (state.key + 1) "" ""


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
    { cidr : Maybe Cidr
    , cidrState : InputState
    , subtrahend : Maybe Cidr
    , subtrahendState : InputState
    , subtraction : SubtractionModel
    }


model : Model
model =
    { cidr = Nothing
    , cidrState = InputState 0 "" ""
    , subtrahend = Nothing
    , subtrahendState = InputState 0 "" ""
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
                ( cidr, state ) =
                    updateInput Cidr.fromString value model.cidrState
            in
            { model | cidr = cidr, cidrState = state }

        SubtrahendInput value ->
            let
                ( cidr, state ) =
                    updateInput Cidr.fromString value model.subtrahendState
            in
            { model | subtrahend = cidr, subtrahendState = state }

        -- TODO(bkeyes): take two args or use Maybe.map2 on model?
        Subtract minuend subtrahend ->
            { model
                | subtraction = subtract minuend subtrahend model.subtraction
                , subtrahend = Nothing
                , subtrahendState = resetInput model.subtrahendState
            }



-- COMPONENTS --


cidrInput : (String -> Msg) -> InputState -> Html Msg
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
            case model.subtrahend of
                Just cidr ->
                    button [ type_ "button", onClick (Subtract minuend cidr) ] [ text "Subtract" ]

                Nothing ->
                    button [ type_ "button", disabled True ] [ text "Subtract" ]
    in
    div []
        [ h2 [] [ text "Subtract" ]
        , form []
            [ cidrInput SubtrahendInput model.subtrahendState
            , submitButton
            ]
        , result
        ]


view : Model -> Html Msg
view model =
    let
        input =
            cidrInput CidrInput model.cidrState
    in
    case model.cidr of
        Just cidr ->
            div []
                [ input
                , cidrInfo cidr
                , subtractor cidr model
                ]

        Nothing ->
            div [] [ input ]
