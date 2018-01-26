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

-- TODO(bkeyes): convert this to an opaque, internal type?
type Cidr = Cidr Ip Int

ipFromString : String -> Result String Ip
ipFromString addr =
    let
        checkOctet : Int -> Result String Int
        checkOctet i =
            if i > 255 || i < 0
                then Err "invalid address (octet out of range)"
                else Ok i

        mergeOctets : Int -> List String -> Result String Ip
        mergeOctets ip octets =
            case octets of
                [] ->
                    Ok ip

                octet :: rest ->
                    String.toInt octet
                        |> Result.andThen checkOctet
                        |> Result.map (\i -> Bitwise.or ip (Bitwise.shiftLeftBy (8 * List.length rest) i))
                        |> Result.andThen (\ip -> mergeOctets ip rest)


        octets = String.split "." addr
    in
        if List.length octets == 4
            then mergeOctets 0 octets
            else Err "invalid address (too many octets)"

ipToString : Ip -> String
ipToString ip =
    let
        octetToString i =
            toString (Bitwise.and 0xFF (Bitwise.shiftRightBy i ip))
    in
        String.join "." (List.map octetToString [24, 16, 8, 0])


applyMask : Int -> Ip -> Ip
applyMask mask addr =
    Bitwise.and addr (Bitwise.shiftLeftBy (32 - mask) 0xFFFFFFFF)

cidrFromString : String -> Result String Cidr
cidrFromString s =
    let
        split : String -> Result String (String, String)
        split s =
            case String.split "/" s of
                [addr, mask] -> Ok (addr, mask)
                _ -> Err "invalid CIDR block (bad format)"

        checkMask : Int -> Result String Int
        checkMask m =
            if m > 32 || m < 0
                then Err "invalid CIDR block (mask out of range)"
                else Ok m

        parseMask : String -> Result String Int
        parseMask mask =
            String.toInt mask |> Result.andThen checkMask

        cidr : Ip -> Int -> Cidr
        cidr addr mask =
            Cidr (applyMask mask addr) mask
    in
        split s
            |> Result.andThen (\(addr, mask) -> Result.map2 cidr (ipFromString addr) (parseMask mask))

cidrToString : Cidr -> String
cidrToString (Cidr addr mask) =
    (ipToString addr) ++ "/" ++ (toString mask)


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
        [ text (cidrToString (Result.withDefault (Cidr 0 0) (cidrFromString "192.168.1.4/31")))
        ]
