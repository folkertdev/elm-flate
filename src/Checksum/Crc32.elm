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

        -- Because of the bitwise and, this is actually completely safe
        unsafeGet i arr =
            case Array.get i arr of
                Nothing ->
                    0

                Just v ->
                    v
    in
    if length == 0 then
        0

    else
        let
            go ( i, crc ) =
                if i < length then
                    Decode.unsignedInt8
                        |> Decode.map
                            (\byte ->
                                let
                                    a =
                                        Bitwise.xor crc byte
                                            |> Bitwise.shiftRightZfBy 0

                                    b =
                                        unsafeGet (Bitwise.and a 0x0F) tinf_crc32tab
                                            |> Bitwise.xor (Bitwise.shiftRightZfBy 4 a)
                                            |> Bitwise.shiftRightZfBy 0

                                    c =
                                        unsafeGet (Bitwise.and b 0x0F) tinf_crc32tab
                                            |> Bitwise.xor (Bitwise.shiftRightZfBy 4 b)
                                in
                                Decode.Loop ( i + 1, c )
                            )

                else
                    Bitwise.xor crc 0xFFFFFFFF
                        |> Bitwise.shiftRightZfBy 0
                        |> Decode.Done
                        |> Decode.succeed
        in
        Decode.decode (Decode.loop ( 0, initialCrc ) go) buffer
            |> Maybe.withDefault 0


tinf_crc32tab =
    Array.fromList
        [ 0x00
        , 0x1DB71064
        , 0x3B6E20C8
        , 0x26D930AC
        , 0x76DC4190
        , 0x6B6B51F4
        , 0x4DB26158
        , 0x5005713C
        , 0xEDB88320
        , 0xF00F9344
        , 0xD6D6A3E8
        , 0xCB61B38C
        , 0x9B64C2B0
        , 0x86D3D2D4
        , 0xA00AE278
        , 0xBDBDF21C
        ]
