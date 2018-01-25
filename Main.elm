module Main exposing (..)

import Html exposing (..)
import Bitwise


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }

-- CIDR HACKS

type alias Ip = Int

type Cidr = Cidr Ip Int

cidrFromString : String -> Result String Cidr
cidrFromString s =
    let 
        split : String -> Result String (String, String)
        split s = 
            case String.split "/" s of
                [addr, block] ->
                    Ok (addr, block)
                _ ->
                    Err "invalid CIDR block"

        parseAddr : String -> Result String Ip
        parseAddr addr =
            let
                parts = String.split "." addr

                reduce : Int -> List String -> Result String Ip -> Result String Ip
                reduce shift parts res =
                    case res of
                        Ok ip ->
                            case parts of
                                [] ->
                                    Ok ip

                                part :: rest ->
                                    String.toInt part 
                                        |> Result.map (\i -> (Bitwise.or ip (Bitwise.shiftLeftBy shift i)))
                                        |> Result.andThen (\i -> reduce (shift - 8) rest (Ok i))

                        _ -> res
            in
                if List.length parts == 4
                    then reduce 24 parts (Ok 0)
                    else Err "invalid address"

        parseBlock : String -> Result String Int
        parseBlock = String.toInt

        combine : Result String Ip -> Result String Int -> Result String Cidr
        combine addr block =
            Result.map2 (Cidr) addr block
    in
        split s
            |> Result.andThen (\(a, b) -> combine (parseAddr a) (parseBlock b))


-- END CIDR HACKS


type alias Model =
    { cidr : Cidr
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Cidr 0 0), Cmd.none )


type Msg
    = DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text "placeholder"
        ]
