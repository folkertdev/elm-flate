module Checksum.Adler32 exposing (adler32)

import Array
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode


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


processChunk : ( Int, Int ) -> Bytes -> Maybe ( Int, Int )
processChunk ( is1, is2 ) buffer =
    let
        length =
            Bytes.width buffer

        go ( i, s1, s2 ) =
            if i < length then
                Decode.unsignedInt8
                    |> Decode.map
                        (\byte ->
                            let
                                newS1 =
                                    s1 + byte

                                newS2 =
                                    s2 + newS1
                            in
                            Decode.Loop ( i + 1, newS1, newS2 )
                        )

            else
                let
                    newS1 =
                        s1 |> modBy a32.base

                    newS2 =
                        s2 |> modBy a32.base
                in
                Decode.succeed (Decode.Done ( newS1, newS2 ))
    in
    Decode.decode (Decode.loop ( 0, is1, is2 ) go) buffer


adler32 : Bytes -> Int
adler32 =
    tinf_adler32


tinf_adler32 : Bytes -> Int
tinf_adler32 buffer =
    let
        go s1 s2 slices =
            case slices of
                [] ->
                    Just (Bitwise.or (Bitwise.shiftLeftBy 16 s2) s1)

                first :: rest ->
                    case processChunk ( s1, s2 ) first of
                        Nothing ->
                            Nothing

                        Just ( newS1, newS2 ) ->
                            go newS1 newS2 rest
    in
    slice a32.nmax buffer
        |> go 1 0
        |> Maybe.withDefault 0
        |> Bitwise.shiftRightZfBy 0
