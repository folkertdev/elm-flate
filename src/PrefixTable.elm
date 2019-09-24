module PrefixTable exposing (Prefix(..), PrefixCode, PrefixTable, insert, new, prefixAt)

import Array exposing (Array)
import Bitwise
import Dict exposing (Dict)
import Experimental.ByteArray as ByteArray exposing (ByteArray)


{-| Maximum backward distance of a pointer.
-}
max_distance =
    32768


{-| Maximum size of a sliding window.
-}
max_window_size =
    max_distance


type PrefixTable
    = Small (Dict Int Int)
    | Large LargePrefixTable


type PrefixCode
    = PrefixCode Int


new : Int -> PrefixTable
new nbytes =
    if nbytes < max_window_size then
        Small Dict.empty

    else
        Large newLargePrefixTable


insert : PrefixCode -> Int -> PrefixTable -> ( PrefixTable, Maybe Int )
insert (PrefixCode prefix_) position ptable =
    let
        prefix =
            Bitwise.shiftRightZfBy 0 prefix_
                |> Bitwise.and 0x00FFFFFF
    in
    case ptable of
        Small dict ->
            case Dict.get prefix dict of
                Nothing ->
                    ( Small (Dict.insert prefix position dict), Nothing )

                Just oldValue ->
                    ( Small (Dict.insert prefix position dict), Just oldValue )

        Large (LargePrefixTable array) ->
            let
                index =
                    Bitwise.shiftRightBy 8 prefix
            in
            case Array.get index array of
                Nothing ->
                    ( ptable, Nothing )

                Just positions ->
                    insertInList index array (Bitwise.and 0xFF prefix) position positions []


insertInList i array p2 position remaining accum =
    case remaining of
        [] ->
            let
                newPositions =
                    ( p2, position ) :: accum
            in
            ( Large (LargePrefixTable (Array.set i newPositions array)), Nothing )

        (( key, oldValue ) as current) :: rest ->
            if (key - p2) == 0 then
                let
                    newPositions =
                        accum ++ (( key, position ) :: rest)
                in
                ( Large (LargePrefixTable (Array.set i newPositions array)), Just oldValue )

            else
                insertInList i array p2 position rest (current :: accum)


type LargePrefixTable
    = LargePrefixTable (Array (List ( Int, Int )))


newLargePrefixTable =
    LargePrefixTable (Array.repeat 0xFFFF [])



-- create prefixes


type Prefix
    = Prefix Int PrefixCode
    | Trailing1 Int
    | Trailing2 Int Int
    | OutOfBounds


{-| Create a "hash" for a position, based on the byte and two following bytes.

This function uses lowlevel ByteArray access to minimize the number of `Array.get`s

-}
prefixAt : Int -> ByteArray -> Prefix
prefixAt k input =
    let
        size =
            ByteArray.length input
    in
    if k + 2 >= size then
        if k >= size then
            OutOfBounds

        else if k + 1 >= size then
            case ByteArray.get k input of
                Nothing ->
                    OutOfBounds

                Just value ->
                    Trailing1 value

        else
            case ByteArray.get k input of
                Nothing ->
                    OutOfBounds

                Just v1 ->
                    case ByteArray.get (k + 1) input of
                        Nothing ->
                            OutOfBounds

                        Just v2 ->
                            Trailing2 v1 v2

    else
        -- all within bounds
        let
            offset =
                k
                    |> remainderBy 4

            internalIndex =
                k // 4
        in
        case offset of
            0 ->
                case ByteArray.getInt32 internalIndex input of
                    Nothing ->
                        OutOfBounds

                    Just int32 ->
                        let
                            code =
                                Bitwise.shiftRightBy 8 int32

                            first =
                                Bitwise.shiftRightBy 24 int32
                                    |> Bitwise.shiftRightZfBy 0
                                    |> Bitwise.and 0xFF
                        in
                        Prefix first (PrefixCode code)

            1 ->
                case ByteArray.getInt32 internalIndex input of
                    Nothing ->
                        OutOfBounds

                    Just int32 ->
                        let
                            code =
                                Bitwise.and 0x00FFFFFF int32

                            first =
                                Bitwise.shiftRightBy 16 int32
                                    |> Bitwise.and 0xFF
                                    |> Bitwise.shiftRightZfBy 0
                                    |> Bitwise.and 0xFF
                        in
                        Prefix first <| PrefixCode code

            2 ->
                case ByteArray.getInt32 internalIndex input of
                    Nothing ->
                        OutOfBounds

                    Just int32 ->
                        case ByteArray.getInt32 (internalIndex + 1) input of
                            Nothing ->
                                OutOfBounds

                            Just nextInt32 ->
                                let
                                    code =
                                        Bitwise.and 0xFFFF int32
                                            |> Bitwise.shiftLeftBy 8
                                            |> Bitwise.or (Bitwise.shiftRightBy 24 nextInt32 |> Bitwise.and 0xFF)
                                            |> Bitwise.shiftRightZfBy 0
                                            |> Bitwise.and 0x00FFFFFF

                                    first =
                                        Bitwise.shiftRightBy 8 int32
                                            |> Bitwise.and 0xFF
                                            |> Bitwise.shiftRightZfBy 0
                                            |> Bitwise.and 0xFF
                                in
                                Prefix first <| PrefixCode code

            _ ->
                case ByteArray.getInt32 internalIndex input of
                    Nothing ->
                        OutOfBounds

                    Just int32 ->
                        case ByteArray.getInt32 (internalIndex + 1) input of
                            Nothing ->
                                OutOfBounds

                            Just nextInt32 ->
                                let
                                    code =
                                        Bitwise.and 0xFF int32
                                            |> Bitwise.shiftLeftBy 16
                                            |> Bitwise.or (Bitwise.shiftRightBy 16 nextInt32 |> Bitwise.and 0xFFFF)

                                    first =
                                        Bitwise.and 0xFF int32
                                            |> Bitwise.shiftRightZfBy 0
                                            |> Bitwise.and 0xFF
                                in
                                Prefix first <| PrefixCode code
