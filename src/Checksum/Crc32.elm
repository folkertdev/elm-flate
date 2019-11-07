module Checksum.Crc32 exposing (crc32)

import Array
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode


crc32 : Bytes -> Int
crc32 =
    tinf_crc32


{-| Implementation note:

javascript uses a signed 32 bit integer, which means that something like

~5 = -6

To get rid of the minus, and get the value that other c-like languages expect here,
we can use `shiftRightZfBy 0` or in general `shiftRightZfBy`

-}
tinf_crc32 : Bytes -> Int
tinf_crc32 buffer =
    let
        length =
            Bytes.width buffer

        initialCrc =
            0xFFFFFFFF
    in
    if length == 0 then
        0

    else
        Decode.decode (Decode.loop { remaining = length, crc = initialCrc } crc32Help) buffer
            |> Maybe.withDefault 0


crc32Help { remaining, crc } =
    if remaining >= 8 then
        Decode.map2
            (\word1 word2 ->
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
                Decode.Loop
                    { remaining = remaining - 8
                    , crc =
                        -- potential speedup: inline these calls
                        crc
                            |> step byte1
                            |> step byte2
                            |> step byte3
                            |> step byte4
                            |> step byte5
                            |> step byte6
                            |> step byte7
                            |> step byte8
                    }
            )
            (Decode.unsignedInt32 Bytes.BE)
            (Decode.unsignedInt32 Bytes.BE)

    else if remaining > 0 then
        Decode.unsignedInt8
            |> Decode.map
                (\byte ->
                    Decode.Loop { remaining = remaining - 1, crc = step byte crc }
                )

    else
        Bitwise.xor crc 0xFFFFFFFF
            |> Bitwise.shiftRightZfBy 0
            |> Decode.Done
            |> Decode.succeed


step byte crc =
    let
        a =
            Bitwise.xor crc byte
                |> Bitwise.shiftRightZfBy 0

        b =
            tinf_crc32case (Bitwise.and a 0x0F)
                |> Bitwise.xor (Bitwise.shiftRightZfBy 4 a)
                |> Bitwise.shiftRightZfBy 0

        c =
            tinf_crc32case (Bitwise.and b 0x0F)
                |> Bitwise.xor (Bitwise.shiftRightZfBy 4 b)
    in
    c


tinf_crc32case i =
    case i of
        0 ->
            0x00

        1 ->
            0x1DB71064

        2 ->
            0x3B6E20C8

        3 ->
            0x26D930AC

        4 ->
            0x76DC4190

        5 ->
            0x6B6B51F4

        6 ->
            0x4DB26158

        7 ->
            0x5005713C

        8 ->
            0xEDB88320

        9 ->
            0xF00F9344

        10 ->
            0xD6D6A3E8

        11 ->
            0xCB61B38C

        12 ->
            0x9B64C2B0

        13 ->
            0x86D3D2D4

        14 ->
            0xA00AE278

        _ ->
            0xBDBDF21C
