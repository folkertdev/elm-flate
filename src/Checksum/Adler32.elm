module Checksum.Adler32 exposing (adler32)

import Array
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)


a32 =
    { base = 65521, nmax = 5552 }


slice : Int -> Bytes -> List Bytes
slice maxSize buffer =
    let
        go ( remainingLength, accum ) =
            if remainingLength == 0 then
                Decode.succeed (Decode.Done (List.reverse accum))

            else if remainingLength < maxSize then
                Decode.bytes remainingLength
                    |> Decode.map (\final -> Decode.Done (List.reverse (final :: accum)))

            else
                Decode.bytes maxSize
                    |> Decode.map (\elem -> Decode.Loop ( remainingLength - maxSize, elem :: accum ))
    in
    Decode.decode (Decode.loop ( Bytes.width buffer, [] ) go) buffer
        |> Maybe.withDefault []


slice2 : { bufferSize : Int, maxBlockSize : Int } -> Decoder { s1 : Int, s2 : Int }
slice2 { bufferSize, maxBlockSize } =
    let
        go { remainingLength, s1, s2 } =
            if remainingLength == 0 then
                Decode.succeed (Decode.Done { s1 = s1, s2 = s2 })

            else if remainingLength < maxBlockSize then
                helper { remaining = remainingLength, s1 = s1, s2 = s2 }
                    |> Decode.map Decode.Done

            else
                helper { remaining = maxBlockSize, s1 = s1, s2 = s2 }
                    |> Decode.map (\result -> Decode.Loop { remainingLength = remainingLength - maxBlockSize, s1 = result.s1, s2 = result.s2 })
    in
    Decode.loop { remainingLength = bufferSize, s1 = 1, s2 = 0 } go


helper : { remaining : Int, s1 : Int, s2 : Int } -> Decoder { s1 : Int, s2 : Int }
helper config =
    Decode.loop config processChunkHelp


processChunk : { s1 : Int, s2 : Int } -> Bytes -> Maybe { s1 : Int, s2 : Int }
processChunk { s1, s2 } buffer =
    let
        length =
            Bytes.width buffer
    in
    Decode.decode (helper { remaining = length, s1 = s1, s2 = s2 }) buffer


processChunkHelp { remaining, s1, s2 } =
    case remaining of
        0 ->
            let
                newS1 =
                    s1 |> remainderBy a32.base

                newS2 =
                    s2 |> remainderBy a32.base
            in
            Decode.succeed (Decode.Done { s1 = newS1, s2 = newS2 })

        _ ->
            if remaining >= 8 then
                Decode.map2 (step8Bytes remaining s1 s2)
                    (Decode.unsignedInt32 Bytes.BE)
                    (Decode.unsignedInt32 Bytes.BE)

            else
                Decode.unsignedInt8
                    |> Decode.map
                        (\byte ->
                            let
                                newS1 =
                                    s1 + byte

                                newS2 =
                                    s2 + newS1
                            in
                            Decode.Loop { remaining = remaining - 1, s1 = newS1, s2 = newS2 }
                        )


adler32 : Bytes -> Int
adler32 buffer =
    if False then
        let
            go s1 s2 slices =
                case slices of
                    [] ->
                        Just (Bitwise.or (Bitwise.shiftLeftBy 16 s2) s1)

                    first :: rest ->
                        case processChunk { s1 = s1, s2 = s2 } first of
                            Nothing ->
                                Nothing

                            Just result ->
                                go result.s1 result.s2 rest
        in
        slice a32.nmax buffer
            |> go 1 0
            |> Maybe.withDefault 0
            |> Bitwise.shiftRightZfBy 0

    else
        case Decode.decode (slice2 { bufferSize = Bytes.width buffer, maxBlockSize = a32.nmax }) buffer of
            Nothing ->
                0

            Just { s1, s2 } ->
                Bitwise.or (Bitwise.shiftLeftBy 16 s2) s1
                    |> Bitwise.shiftRightZfBy 0


adler32HelpHelp : Bytes -> { s1 : Int, s2 : Int } -> { s1 : Int, s2 : Int }
adler32HelpHelp first input =
    case processChunk input first of
        Nothing ->
            input

        Just r ->
            r


step8Bytes : Int -> Int -> Int -> Int -> Int -> Decode.Step { remaining : Int, s1 : Int, s2 : Int } x
step8Bytes remaining s1 s2 word1 word2 =
    let
        byte1 =
            Bitwise.shiftRightZfBy 24 word1 |> Bitwise.and 0xFF

        byte2 =
            Bitwise.shiftRightZfBy 16 word1 |> Bitwise.and 0xFF

        byte3 =
            Bitwise.shiftRightZfBy 8 word1 |> Bitwise.and 0xFF

        byte4 =
            Bitwise.and 0xFF word1

        byte5 =
            Bitwise.shiftRightZfBy 24 word2 |> Bitwise.and 0xFF

        byte6 =
            Bitwise.shiftRightZfBy 16 word2 |> Bitwise.and 0xFF

        byte7 =
            Bitwise.shiftRightZfBy 8 word2 |> Bitwise.and 0xFF

        byte8 =
            Bitwise.and 0xFF word2
    in
    let
        -- 1
        s1a_1 =
            s1 + byte1

        s2a_1 =
            s2 + s1a_1

        -- 2
        s1a_2 =
            s1a_1 + byte2

        s2a_2 =
            s2a_1 + s1a_2

        -- 3
        s1a_3 =
            s1a_2 + byte3

        s2a_3 =
            s2a_2 + s1a_3

        -- 3
        s1a_4 =
            s1a_3 + byte4

        s2a_4 =
            s2a_3 + s1a_4
    in
    let
        -- 1
        s1b_1 =
            s1a_4 + byte5

        s2b_1 =
            s2a_4 + s1b_1

        -- 2
        s1b_2 =
            s1b_1 + byte6

        s2b_2 =
            s2b_1 + s1b_2

        -- 3
        s1b_3 =
            s1b_2 + byte7

        s2b_3 =
            s2b_2 + s1b_3

        -- 3
        s1b_4 =
            s1b_3 + byte8

        s2b_4 =
            s2b_3 + s1b_4
    in
    Decode.Loop { remaining = remaining - 8, s1 = s1b_4, s2 = s2b_4 }
