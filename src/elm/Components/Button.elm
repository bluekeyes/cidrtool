module Components.Button
    exposing
        ( Options
        , Style(..)
        , Type(..)
        , buttonWithOptions
        , defaultOptions
        )

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


type Type
    = Button
    | Submit


type Style
    = Normal
    | Outline


type alias Options msg =
    { type_ : Type
    , style : Style
    , disabled : Bool
    , attrs : List (Html.Attribute msg)
    }


defaultOptions : Options msg
defaultOptions =
    { type_ = Button
    , style = Normal
    , disabled = False
    , attrs = []
    }


buttonWithOptions : Options msg -> Maybe msg -> String -> Html msg
buttonWithOptions opts onClick text =
    let
        classlist =
            Just
                (Attr.classList
                    [ ( "font-bold py-2 px-3 rounded", True )
                    , ( "bg-blue text-white", opts.style == Normal )
                    , ( "border-blue border-2 text-blue", opts.style == Outline )
                    , ( "opacity-50 cursor-not-allowed", opts.disabled )
                    , ( "hover:bg-blue-dark", opts.style == Normal && not opts.disabled )
                    , ( "hover:bg-blue hover:text-white", opts.style == Outline && not opts.disabled )
                    ]
                )

        buttonAttrs =
            attrs
                [ typeToAttr opts.type_
                , disabledToAttr opts.disabled
                , Maybe.map Events.onClick onClick
                , classlist
                ]
    in
    Html.button (buttonAttrs ++ opts.attrs) [ Html.text text ]


typeToAttr : Type -> Maybe (Html.Attribute msg)
typeToAttr type_ =
    case type_ of
        Button ->
            Just (Attr.type_ "button")

        Submit ->
            Just (Attr.type_ "submit")


disabledToAttr : Bool -> Maybe (Html.Attribute msg)
disabledToAttr disabled =
    if disabled then
        Just (Attr.disabled True)
    else
        Nothing


attrs : List (Maybe (Html.Attribute msg)) -> List (Html.Attribute msg)
attrs =
    List.filterMap identity
