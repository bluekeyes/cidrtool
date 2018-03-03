module Main exposing (..)

import Button
import Cidr exposing (Cidr, Ip)
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, placeholder, type_)
import Html.Events as Events
import ParsedInput exposing (Validity(..))
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
    { cidr : Maybe Cidr

    -- subtraction state
    , subtrahend : Maybe Cidr
    , subtraction : SubtractionModel

    -- input states
    , cidrInput : ParsedInput.Model
    , subtrahendInput : ParsedInput.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { cidr = Nothing
      , subtrahend = Nothing
      , subtraction = SubtractionModel [] []
      , cidrInput = ParsedInput.init
      , subtrahendInput = ParsedInput.init
      }
    , Cmd.none
    )


type CidrInput
    = Primary
    | Subtrahend


type Msg
    = NewCidr (Maybe Cidr)
    | NewSubtrahend (Maybe Cidr)
    | InputMsg CidrInput (ParsedInput.Msg Cidr)
    | Subtract Cidr Cidr
    | ResetSubtraction


cidrConfig : ParsedInput.Config Cidr Msg
cidrConfig =
    let
        inputAttrs validity =
            [ placeholder "0.0.0.0/0"
            , class "block w-full py-2 appearance-none no-outline bg-white text-4xl text-center border-b-8 border-gray-light rounded-sm"
            , classList
                [ ( "text-gray-darkest hover:border-blue focus:border-blue", validity == Valid )
                , ( "text-red-dark hover:border-red-dark focus:border-red-dark", validity == Invalid )
                ]
            ]

        error err =
            case err of
                Nothing ->
                    text ""

                Just e ->
                    span
                        [ class "block relative p-2 mt-4 arrow-tc arrow-red-dark arrow-3 rounded shadow-md text-sm text-white text-center bg-red-dark" ]
                        [ text e ]
    in
    ParsedInput.config
        { parser = Cidr.fromString
        , onValue = NewCidr
        , onMsg = InputMsg Primary
        , customizations =
            { attrs = [ class "w-full" ]
            , inputAttrs = inputAttrs
            , error = error
            }
        }


subtrahendConfig : ParsedInput.Config Cidr Msg
subtrahendConfig =
    let
        inputAttrs validity =
            [ placeholder "0.0.0.0/0"
            , class "w-full py-2 appearance-none text-lg text-center rounded-sm border border-gray-light"
            , classList
                [ ( "text-gray-darkest", validity == Valid )
                , ( "text-red-dark", validity == Invalid )
                ]
            ]

        error err =
            case err of
                Nothing ->
                    text ""

                Just e ->
                    span
                        [ class "block absolute pin-x pin-u p-2 mt-3 arrow-tc arrow-red-dark arrow-2 rounded shadow-md text-sm text-white text-center bg-red-dark" ]
                        [ text e ]
    in
    ParsedInput.config
        { parser = Cidr.fromString
        , onValue = NewSubtrahend
        , onMsg = InputMsg Subtrahend
        , customizations =
            { attrs =
                [ class "relative mr-2 flex-1"
                ]
            , inputAttrs = inputAttrs
            , error = error
            }
        }


send : Msg -> Cmd Msg
send msg =
    Task.perform identity (Task.succeed msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCidr cidr ->
            ( { model | cidr = cidr }, send ResetSubtraction )

        NewSubtrahend cidr ->
            ( { model | subtrahend = cidr }, Cmd.none )

        InputMsg input msg ->
            case input of
                Primary ->
                    ParsedInput.update cidrConfig msg model.cidrInput
                        |> Tuple.mapFirst (\m -> { model | cidrInput = m })

                Subtrahend ->
                    ParsedInput.update subtrahendConfig msg model.subtrahendInput
                        |> Tuple.mapFirst (\m -> { model | subtrahendInput = m })

        Subtract minuend subtrahend ->
            ( { model
                | subtraction = subtract minuend subtrahend model.subtraction
                , subtrahend = Nothing
                , subtrahendInput = ParsedInput.reset model.subtrahendInput
              }
            , Cmd.none
            )

        ResetSubtraction ->
            ( { model
                | subtraction = SubtractionModel [] []
                , subtrahend = Nothing
                , subtrahendInput = ParsedInput.reset model.subtrahendInput
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
                    model.subtrahend
                        |> Maybe.map (always False)
                        |> Maybe.withDefault True
            in
            Button.view
                { style = Button.Normal
                , disabled = isDisabled
                , attrs = [ type_ "submit", class "mr-2" ]
                }
                "Subtract"

        resetButton =
            let
                isDisabled =
                    List.isEmpty model.subtraction.subtrahends
            in
            Button.view
                { style = Button.Border
                , disabled = isDisabled
                , attrs = [ type_ "button", Events.onClick ResetSubtraction ]
                }
                "Reset"

        subtrahendForm =
            let
                attrs =
                    case model.subtrahend of
                        Just cidr ->
                            [ Events.onSubmit (Subtract minuend cidr) ]

                        Nothing ->
                            []
            in
            form (class "flex flex-row mb-4" :: attrs)
                [ ParsedInput.view subtrahendConfig model.subtrahendInput
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
            , ParsedInput.view cidrConfig model.cidrInput
            ]
        ]


view : Model -> Html Msg
view model =
    let
        children =
            case model.cidr of
                Just cidr ->
                    [ appHeader model
                    , cidrInfo cidr
                    , subtractor cidr model
                    ]

                Nothing ->
                    [ appHeader model ]
    in
    div [ class "max-w-sm mx-auto flex flex-col items-center" ] children
