module Main exposing (..)

import Cidr exposing (Cidr, Ip)
import Components.Button as Button exposing (buttonWithOptions)
import Components.CidrInput as CidrInput
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, placeholder, type_)
import Html.Events as Events
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
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
    { cidr : CidrInput.Model
    , subtrahend : CidrInput.Model
    , subtraction : SubtractionModel
    }


init : ( Model, Cmd Msg )
init =
    ( { cidr = CidrInput.init
      , subtrahend = CidrInput.init
      , subtraction = SubtractionModel [] []
      }
    , Cmd.none
    )


type Msg
    = CidrMsg CidrInput.Msg
    | SubtrahendMsg CidrInput.Msg
    | Subtract Cidr Cidr
    | ResetSubtraction


send : Msg -> Cmd Msg
send msg =
    Task.perform identity (Task.succeed msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CidrMsg msg ->
            let
                ( cidr, cmd ) =
                    CidrInput.update msg model.cidr
            in
            ( { model | cidr = cidr }, Cmd.map CidrMsg cmd )

        SubtrahendMsg msg ->
            let
                ( subtrahend, cmd ) =
                    CidrInput.update msg model.subtrahend
            in
            ( { model | subtrahend = subtrahend }, Cmd.map SubtrahendMsg cmd )

        Subtract minuend subtrahend ->
            ( { model
                | subtraction = subtract minuend subtrahend model.subtraction
                , subtrahend = CidrInput.reset model.subtrahend
              }
            , Cmd.none
            )

        ResetSubtraction ->
            ( { model
                | subtraction = SubtractionModel [] []
                , subtrahend = CidrInput.reset model.subtrahend
              }
            , Cmd.none
            )



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


subtractor : Cidr -> Model -> Html Msg
subtractor minuend model =
    let
        subtrahend s =
            [ span [ class "text-xl" ] [ text "−" ]
            , b [ class "text-xl font-normal" ] [ text (Cidr.toString s) ]
            ]

        operands =
            div [ class "mb-4 px-4 text-right border-b-2 border-gray-darkest expr-grid" ]
                (b [ class "text-3xl font-bold" ] [ text (Cidr.toString minuend) ]
                    :: List.concatMap subtrahend (List.reverse model.subtraction.subtrahends)
                )

        result =
            let
                item cidr =
                    li [ class "mb-1" ] [ text (Cidr.toString cidr) ]
            in
            if List.isEmpty model.subtraction.result then
                div [ class "italic text-center" ] [ text "Nothing" ]
            else
                ol [ class "list-reset px-4 text-lg text-right" ] (List.map item model.subtraction.result)

        submitButton =
            let
                isDisabled =
                    CidrInput.getValue model.subtrahend
                        |> Maybe.map (always False)
                        |> Maybe.withDefault True

                opts =
                    Button.defaultOptions
            in
            buttonWithOptions
                { opts
                    | style = Button.Normal
                    , type_ = Button.Submit
                    , disabled = isDisabled
                    , attrs = [ class "mr-2" ]
                }
                Nothing
                "Subtract"

        resetButton =
            let
                isDisabled =
                    List.isEmpty model.subtraction.subtrahends

                opts =
                    Button.defaultOptions
            in
            buttonWithOptions
                { opts | style = Button.Outline, disabled = isDisabled }
                (Just ResetSubtraction)
                "Reset"

        subtrahendForm =
            let
                attrs =
                    case CidrInput.getValue model.subtrahend of
                        Just cidr ->
                            [ Events.onSubmit (Subtract minuend cidr) ]

                        Nothing ->
                            []
            in
            form (class "flex flex-row mb-4" :: attrs)
                [ subtrahendInput model
                , submitButton
                , resetButton
                ]
    in
    div [ class "mb-4 p-4 bg-white shadow rounded-sm w-full" ]
        [ h2 [ class "pb-4 mb-4 text-2xlg text-center leading-none border-b border-gray-light" ] [ text "Subtract" ]
        , subtrahendForm
        , div [ class "px-12" ]
            (if List.isEmpty model.subtraction.subtrahends then
                []
             else
                [ operands, result ]
            )
        ]


subtrahendInput : Model -> Html Msg
subtrahendInput model =
    CidrInput.view
        { style = CidrInput.Normal, onInput = SubtrahendMsg }
        [ class "relative mr-2 flex-1" ]
        model.subtrahend


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
                    , subtractor cidr model
                    ]

                Nothing ->
                    [ appHeader model ]
    in
    div [ class "max-w-sm mx-auto flex flex-col items-center" ] children
