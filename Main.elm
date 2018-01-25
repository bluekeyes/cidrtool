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
                [addr, block] -> Ok (addr, block)
                _ -> Err "invalid CIDR block"

        mergeOctets : Int -> List String -> Result String Ip
        mergeOctets ip octets =
            case octets of
                [] ->
                    Ok ip

                octet :: rest ->
                    String.toInt octet
                        |> Result.map (\i -> (Bitwise.or ip (Bitwise.shiftLeftBy (8 * List.length rest) i)))
                        |> Result.andThen (\ip -> mergeOctets ip rest)


        parseAddr : String -> Result String Ip
        parseAddr addr =
            let
                octets = String.split "." addr
            in
                if List.length octets == 4
                    then mergeOctets 0 octets
                    else Err "invalid address"

        parseBlock : String -> Result String Int
        parseBlock = String.toInt
    in
        split s
            |> Result.andThen (\(a, b) -> Result.map2 Cidr (parseAddr a) (parseBlock b))
            -- TODO(bkeyes): mask addr with block


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
