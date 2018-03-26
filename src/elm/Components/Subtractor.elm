module Components.Subtractor
    exposing
        ( Model
        , Msg
        , init
        , reset
        , update
        , view
        )

import Cidr exposing (Cidr)
import Components.CidrInput as CidrInput
import Html exposing (Attribute, Html, b, button, div, h2, li, ol, span, text)
import Html.Attributes exposing (class, disabled, type_)
import Html.Events as Event


type alias Model =
    { input : CidrInput.Model
    , subtrahends : List Cidr
    }


type Msg
    = InputMsg CidrInput.Msg
    | Subtract Cidr
    | Reset


init : Model
init =
    { input = CidrInput.init
    , subtrahends = []
    }


reset : Model -> Model
reset model =
    { input = CidrInput.reset model.input, subtrahends = [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputMsg msg ->
            let
                ( input, cmd ) =
                    CidrInput.update msg model.input
            in
            ( { model | input = input }, Cmd.map InputMsg cmd )

        Subtract subtrahend ->
            ( { model
                | input = CidrInput.reset model.input
                , subtrahends = subtrahend :: model.subtrahends
              }
            , Cmd.none
            )

        Reset ->
            ( { input = CidrInput.reset model.input, subtrahends = [] }, Cmd.none )


view : (Msg -> msg) -> Cidr -> Model -> Html msg
view onMsg minuend model =
    let
        subtrahend s =
            [ span [ class "text-xl" ] [ text "−" ]
            , b [ class "text-xl font-normal" ] [ text (Cidr.toString s) ]
            ]

        operands =
            div [ class "mb-4 px-4 text-right border-b-2 border-gray-darkest expr-grid" ]
                (b [ class "text-3xl font-bold" ] [ text (Cidr.toString minuend) ]
                    :: List.concatMap subtrahend (List.reverse model.subtrahends)
                )

        resultList =
            let
                result =
                    Cidr.subtract minuend model.subtrahends

                item cidr =
                    li [ class "mb-1" ] [ text (Cidr.toString cidr) ]
            in
            if List.isEmpty result then
                div [ class "italic text-center" ] [ text "Nothing" ]
            else
                ol [ class "list-reset px-4 text-lg text-right" ] (List.map item result)
    in
    div [ class "mb-4 p-4 bg-white shadow rounded-sm w-full" ]
        [ h2 [ class "pb-4 mb-4 text-2xlg text-center leading-none border-b border-gray-light" ] [ text "Subtract" ]
        , subtrahendForm onMsg model
        , div [ class "px-12" ]
            (if List.isEmpty model.subtrahends then
                []
             else
                [ operands, resultList ]
            )
        ]


subtrahendForm : (Msg -> msg) -> Model -> Html msg
subtrahendForm onMsg { input, subtrahends } =
    let
        subtrahendInput =
            CidrInput.view
                { style = CidrInput.Normal, onInput = onMsg << InputMsg }
                [ class "mr-2 flex-1" ]
                input

        isSubmitDisabled =
            CidrInput.getValue input == Nothing

        isResetDisabled =
            List.isEmpty subtrahends

        attrs =
            case CidrInput.getValue input of
                Just cidr ->
                    [ Event.onSubmit (onMsg (Subtract cidr)) ]

                Nothing ->
                    []
    in
    Html.form
        (class "flex flex-row mb-4" :: attrs)
        [ subtrahendInput
        , submitButton isSubmitDisabled
        , resetButton onMsg isResetDisabled
        ]


submitButton : Bool -> Html msg
submitButton isDisabled =
    button
        [ class "btn mr-2"
        , type_ "submit"
        , disabled isDisabled
        ]
        [ text "Subtract" ]


resetButton : (Msg -> msg) -> Bool -> Html msg
resetButton onMsg isDisabled =
    button
        [ class "btn btn--outline"
        , type_ "button"
        , disabled isDisabled
        , Event.onClick (onMsg Reset)
        ]
        [ text "Reset" ]
