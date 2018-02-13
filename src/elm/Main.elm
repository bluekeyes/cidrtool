module Main exposing (..)

import Cidr exposing (Cidr, Ip)
import Html exposing (..)
import Html.Attributes exposing (class, classList, defaultValue, disabled, placeholder, type_)
import ParsedInput


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
      , cidrInput = ParsedInput.initial
      , subtrahendInput = ParsedInput.initial
      }
    , Cmd.none
    )


type Msg
    = CidrInput (ParsedInput.Msg Cidr)
    | SubtrahendInput (ParsedInput.Msg Cidr)
    | NewCidr (Maybe Cidr)
    | NewSubtrahend (Maybe Cidr)
    | Subtract Cidr Cidr


cidrConfig : ParsedInput.Config Cidr Msg
cidrConfig =
    let
        onView hasError =
            { label = "Enter a CIDR block"
            , labelAttrs = [ class "text-xl font-bold mb-2" ]
            , inputAttrs =
                [ placeholder "0.0.0.0/0"
                , class "w-full py-2 mb-2 no-outline bg-white text-2xl text-center border-b-4 border-blue-lighter"
                , classList
                    [ ( "text-blue-darker focus:border-blue", not hasError )
                    , ( "text-red focus:border-red", hasError )
                    ]
                ]
            , errorAttrs = [ class "text-sm text-red" ]
            }
    in
    { parser = Cidr.fromString
    , onValue = NewCidr
    , onMsg = CidrInput
    , onView = onView
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CidrInput inMsg ->
            let
                ( inModel, cmd ) =
                    ParsedInput.update cidrConfig inMsg model.cidrInput
            in
            ( { model | cidrInput = inModel }, cmd )

        SubtrahendInput inMsg ->
            let
                -- TODO(bkeyes): use real config
                ( inModel, cmd ) =
                    ParsedInput.update cidrConfig inMsg model.subtrahendInput
            in
            ( { model | subtrahendInput = inModel }, cmd )

        NewCidr cidr ->
            ( { model | cidr = cidr }, Cmd.none )

        NewSubtrahend cidr ->
            ( { model | subtrahend = cidr }, Cmd.none )

        -- TODO(bkeyes): take two args or use Maybe.map2 on model?
        Subtract minuend subtrahend ->
            ( { model
                | subtraction = subtract minuend subtrahend model.subtraction
                , subtrahendInput = ParsedInput.reset model.subtrahendInput
              }
            , Cmd.none
            )



-- COMPONENTS --
{-
   cidrInput : (String -> Msg) -> ParsedInput.Model Cidr Msg -> Html Msg
   cidrInput action state =
       form [ class "w-full max-w-md" ]
           [ Html.Keyed.node "div"
               [ class "flex items-center justify-between" ]
               [ ( "label", label [ class "flex-no-shrink min-w-1/4 font-bold" ] [ text "Enter a CIDR block" ] )
               , ( toString state.key
                 , input
                       [ type_ "text", defaultValue state.raw, onInput action, class "no-outline bg-transparent border-b border-grey-dark focus:border-blue w-full text-gray-darker mx-3 py-1 px-2 text-center" ]
                       []
                 )
               , ( "error", small [ class "flex-no-shrink min-w-1/4 text-red" ] [ text state.error ] )
               ]
           ]
-}


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



{-
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
-}


appHeader : Model -> Html Msg
appHeader model =
    let
        input =
            ParsedInput.view
                cidrConfig
                [ class "flex flex-col items-center w-full max-w-sm" ]
                model.cidrInput
    in
    header [ class "w-full my-4 flex flex-col items-center text-blue-darker" ]
        [ h1 [ class "text-5xl mb-6" ] [ text "cidrtool" ]
        , input
        ]


view : Model -> Html Msg
view model =
    case model.cidr of
        Just cidr ->
            div []
                [ appHeader model
                , cidrInfo cidr

                -- , subtractor cidr model
                ]

        Nothing ->
            div [] [ appHeader model ]
