module Main exposing (..)

import Bitwise
import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- CIDR HACKS


type alias Ip =
    Int


type
    Cidr
    -- TODO(bkeyes): convert this to an opaque, internal type?
    = Cidr Ip Int


ipFromString : String -> Result String Ip
ipFromString addr =
    let
        checkOctet : Int -> Result String Int
        checkOctet i =
            if i > 255 || i < 0 then
                Err "invalid address (octet out of range)"
            else
                Ok i

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

        octets =
            String.split "." addr
    in
    if List.length octets == 4 then
        mergeOctets 0 octets
    else
        Err "invalid address (too many octets)"


ipToString : Ip -> String
ipToString ip =
    let
        octetToString i =
            toString (Bitwise.and 0xFF (Bitwise.shiftRightBy i ip))
    in
    String.join "." (List.map octetToString [ 24, 16, 8, 0 ])


makeCidr : Ip -> Int -> Cidr
makeCidr addr bits =
    if bits == 0 then
        Cidr 0 0
    else
        Cidr (Bitwise.and addr (Bitwise.shiftLeftBy (32 - bits) 0xFFFFFFFF)) bits


cidrFromString : String -> Result String Cidr
cidrFromString s =
    let
        split : String -> Result String ( String, String )
        split s =
            case String.split "/" s of
                [ addr, mask ] ->
                    Ok ( addr, mask )

                _ ->
                    Err "invalid CIDR block (bad format)"

        checkMask : Int -> Result String Int
        checkMask m =
            if m > 32 || m < 0 then
                Err "invalid CIDR block (mask out of range)"
            else
                Ok m

        parseMask : String -> Result String Int
        parseMask mask =
            String.toInt mask |> Result.andThen checkMask
    in
    split s
        |> Result.andThen (\( addr, mask ) -> Result.map2 makeCidr (ipFromString addr) (parseMask mask))


cidrToString : Cidr -> String
cidrToString (Cidr addr mask) =
    ipToString addr ++ "/" ++ toString mask


address : Cidr -> Ip
address (Cidr addr _) =
    -- TODO(bkeyes): this function is identical to firstAddress
    addr


maskBits : Cidr -> Int
maskBits (Cidr _ bits) =
    bits


firstAddress : Cidr -> Ip
firstAddress (Cidr addr _) =
    addr


nextAddress : Cidr -> Ip
nextAddress (Cidr addr mask) =
    if mask == 0 then
        0
    else
        addr + Bitwise.shiftLeftBy (32 - mask) 1


lastAddress : Cidr -> Ip
lastAddress cidr =
    nextAddress cidr - 1


inBlock : Cidr -> Ip -> Bool
inBlock cidr ip =
    firstAddress cidr <= ip && ip < nextAddress cidr


intersects : Cidr -> Cidr -> Bool
intersects cidr other =
    let
        test : Cidr -> Cidr -> Bool
        test x y =
            firstAddress x >= firstAddress y && firstAddress x < nextAddress y
    in
    test cidr other || test other cidr


subtract : Cidr -> List Cidr -> List Cidr
subtract minuend subtrahends =
    let
        toRanges : Ip -> Ip -> List Cidr -> List ( Ip, Ip )
        toRanges start end cidrs =
            case cidrs of
                [] ->
                    [ ( start, end ) ]

                c :: rest ->
                    if firstAddress c <= start || nextAddress c <= start then
                        toRanges (max start (nextAddress c)) end rest
                    else
                        ( start, firstAddress c ) :: toRanges (nextAddress c) end rest

        largestFit : Ip -> Ip -> Int -> Maybe Cidr
        largestFit start end bits =
            let
                candidate =
                    makeCidr start bits
            in
            if start >= end then
                Nothing
            else if nextAddress candidate <= end && firstAddress candidate == start then
                Just candidate
            else
                largestFit start end (bits + 1)

        fillRange : Int -> ( Ip, Ip ) -> List Cidr
        fillRange bits ( start, end ) =
            case largestFit start end bits of
                Nothing ->
                    []

                Just cidr ->
                    cidr :: fillRange bits ( nextAddress cidr, end )
    in
    subtrahends
        |> List.sortBy firstAddress
        |> toRanges (firstAddress minuend) (nextAddress minuend)
        |> List.concatMap (fillRange (maskBits minuend))



-- END CIDR HACKS


type alias Model =
    { cidr : Cidr
    , parseError : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Cidr 0 0) "", Cmd.none )


type Msg
    = NewInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewInput input ->
            case cidrFromString input of
                Ok cidr ->
                    ( { model | cidr = cidr, parseError = "" }, Cmd.none )

                Err err ->
                    ( { model | cidr = Cidr 0 0, parseError = err }, Cmd.none )


test : Cidr -> Html Msg
test cidr =
    let
        cidrToRow : Cidr -> Html Msg
        cidrToRow cidr =
            tr []
                [ td [] [ text (cidrToString cidr) ]
                , td [] [ text (ipToString (lastAddress cidr)) ]
                ]

        makeTable : List String -> List (Html Msg) -> Html Msg
        makeTable headers rows =
            table []
                [ thead [] [ tr [] (List.map (\s -> th [] [ text s ]) headers) ]
                , tbody [] rows
                ]
    in
    cidrFromString "10.0.0.0/8"
        |> Result.map (\c -> subtract c [ cidr ])
        |> Result.map (\cs -> makeTable [ "CIDR", "Last IP" ] (List.map cidrToRow cs))
        |> Result.withDefault (div [] [ text "invalid cidr literal in code" ])


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "10.0.0.0/8" ]
        , input [ placeholder "Enter CIDR block", onInput NewInput ] []
        , div [] [ text (cidrToString model.cidr) ]
        , div [] [ text model.parseError ]
        , test model.cidr
        ]
