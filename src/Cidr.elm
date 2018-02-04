module Cidr
    exposing
        ( Cidr
        , Ip
        , firstAddress
        , fromString
        , inBlock
        , intersects
        , ipFromString
        , ipToString
        , lastAddress
        , maskBits
        , nextAddress
        , routingMask
        , size
        , subtract
        , toString
        , zero
        )

{-| Represent and manipulate IPv4 addresses and CIDR blocks.


## IP Addresses

@docs Ip, ipFromString, ipToString


## CIDR Blocks

@docs Cidr, fromString, toString, zero, firstAddress, maskBits, routingMask, nextAddress, lastAddress, size
@docs inBlock, intersects, subtract

-}

import Bitwise


{-| An IPv4 address, stored as a 32-bit integer
-}
type alias Ip =
    Int


{-| A CIDR block.
-}
type Cidr
    = Cidr Ip Int


{-| Returns the zero CIDR block, `0.0.0.0/0`.
-}
zero : Cidr
zero =
    Cidr 0 0


makeCidr : Ip -> Int -> Cidr
makeCidr addr bits =
    if bits == 0 then
        Cidr 0 0
    else
        Cidr (Bitwise.and addr (Bitwise.shiftLeftBy (32 - bits) 0xFFFFFFFF)) bits


{-| Parses a string representation of an IP address.
-}
ipFromString : String -> Result String Ip
ipFromString addr =
    let
        checkOctet : Int -> Result String Int
        checkOctet i =
            if i > 255 || i < 0 then
                Err "Invalid address: octet out of range"
            else
                Ok i

        mergeOctets : Int -> List String -> Result String Ip
        mergeOctets ip octets =
            case octets of
                [] ->
                    Ok ip

                octet :: rest ->
                    String.toInt octet
                        |> Result.mapError (always "Invalid address: octet is not an integer")
                        |> Result.andThen checkOctet
                        |> Result.map (\i -> Bitwise.or ip (Bitwise.shiftLeftBy (8 * List.length rest) i))
                        |> Result.andThen (\ip -> mergeOctets ip rest)

        octets =
            String.split "." addr
    in
    if List.length octets == 4 then
        mergeOctets 0 octets
    else
        Err "Invalid address: wrong number of octets"


{-| Returns a string representation of an IP address.
-}
ipToString : Ip -> String
ipToString ip =
    let
        octetToString i =
            Basics.toString (Bitwise.and 0xFF (Bitwise.shiftRightBy i ip))
    in
    String.join "." (List.map octetToString [ 24, 16, 8, 0 ])


{-| Parses a string representation of a CIDR block.
-}
fromString : String -> Result String Cidr
fromString s =
    let
        split : String -> Result String ( String, String )
        split s =
            case String.split "/" s of
                [ addr ] ->
                    Err "Invalid CIDR block: missing mask"

                [ addr, "" ] ->
                    Err "Invalid CIDR block: missing mask"

                [ addr, mask ] ->
                    Ok ( addr, mask )

                _ ->
                    Err "Invalid CIDR block: too many masks"

        checkMask : Int -> Result String Int
        checkMask m =
            if m > 32 || m < 0 then
                Err "Invalid CIDR block: mask out of range"
            else
                Ok m

        parseMask : String -> Result String Int
        parseMask mask =
            String.toInt mask
                |> Result.mapError (always "Invalid CIDR block: mask is not an integer")
                |> Result.andThen checkMask
    in
    split s
        |> Result.andThen (\( addr, mask ) -> Result.map2 makeCidr (ipFromString addr) (parseMask mask))


{-| Returns a string representation of a CIDR block.
-}
toString : Cidr -> String
toString (Cidr addr bits) =
    ipToString addr ++ "/" ++ Basics.toString bits


{-| Returns the number of leading bits in the CIDR blocks routing mask.
-}
maskBits : Cidr -> Int
maskBits (Cidr _ bits) =
    bits


{-| Returns the routing mask for a CIDR block.
-}
routingMask : Cidr -> Int
routingMask (Cidr _ bits) =
    if bits == 0 then
        0
    else
        Bitwise.shiftLeftBy (32 - bits) 0xFFFFFFFF


{-| Returns the size (number of IP addresses) of the CIDR block.
-}
size : Cidr -> Int
size (Cidr _ bits) =
    2 ^ (32 - bits)


{-| Returns the first IP address in a CIDR block.
-}
firstAddress : Cidr -> Ip
firstAddress (Cidr addr _) =
    addr


{-| Returns the first IP address that comes after a CIDR block.
-}
nextAddress : Cidr -> Ip
nextAddress (Cidr addr bits) =
    if bits == 0 then
        0
    else
        addr + 2 ^ (32 - bits)


{-| Returns the last IP address in a CIDR block.
-}
lastAddress : Cidr -> Ip
lastAddress cidr =
    nextAddress cidr - 1


{-| Tests if an IP is part of a CIDR block.
-}
inBlock : Cidr -> Ip -> Bool
inBlock cidr ip =
    firstAddress cidr <= ip && ip < nextAddress cidr


{-| Tests if two CIDR blocks intersect with each other.
-}
intersects : Cidr -> Cidr -> Bool
intersects cidr other =
    let
        test : Cidr -> Cidr -> Bool
        test x y =
            firstAddress x >= firstAddress y && firstAddress x < nextAddress y
    in
    test cidr other || test other cidr


{-| Subtracts one or more CIDR blocks from another CIDR block.
-}
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
