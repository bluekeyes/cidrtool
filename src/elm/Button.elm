module Button exposing (Config, Style(..), view)

import Html exposing (Html)
import Html.Attributes as Attr


type Style
    = Normal
    | Border


type alias Config msg =
    { style : Style
    , disabled : Bool
    , attrs : List (Html.Attribute msg)
    }


view : Config msg -> String -> Html msg
view config text =
    let
        classlist =
            Attr.classList
                [ ( "font-bold py-2 px-3 rounded", True )
                , ( "bg-blue text-white", config.style == Normal )
                , ( "border-blue border-2 text-blue", config.style == Border )
                , ( "opacity-50 cursor-not-allowed", config.disabled )
                , ( "hover:bg-blue-dark", config.style == Normal && not config.disabled )
                , ( "hover:bg-blue hover:text-white", config.style == Border && not config.disabled )
                ]
    in
    Html.button
        (classlist :: Attr.disabled config.disabled :: config.attrs)
        [ Html.text text ]
