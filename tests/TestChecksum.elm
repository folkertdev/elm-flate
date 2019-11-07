module TestChecksum exposing (suite)

import Array
import ByteArray
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Checksum.Adler32 as Adler32
import Checksum.Crc32 as Crc32
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite =
    describe "Checksum"
        [ adler32, crc32 ]


fuzzBytes : Fuzz.Fuzzer Bytes
fuzzBytes =
    Fuzz.list (Fuzz.map Encode.unsignedInt8 (Fuzz.intRange 0 255))
        |> Fuzz.map (Encode.sequence >> Encode.encode)


adler32 =
    let
        setup expected data =
            test (Debug.toString data) <|
                \_ ->
                    data
                        |> Array.fromList
                        |> ByteArray.toBytes
                        |> Adler32.adler32
                        |> Expect.equal expected
    in
    describe "adler32 checksum"
        [ setup 131074 [ 1 ]
        , setup 196611 [ 2 ]
        , setup 87484452 (List.range 0 1000)
        , setup 2380301274 (List.range 200 2000)
        , setup 88473791 [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 ]
        , setup 1604860569 (List.concat (List.repeat 1000 [ 1, 2, 3, 4, 5 ]))
        , fuzz fuzzBytes "adler32 output is always > 0" <|
            \buffer ->
                Adler32.adler32 buffer
                    |> Expect.atLeast 0
        ]


crc32 =
    let
        setup expected data =
            test (Debug.toString data) <|
                \_ ->
                    data
                        |> Array.fromList
                        |> ByteArray.toBytes
                        |> Crc32.crc32
                        |> Expect.equal expected
    in
    describe "crc32 checksum"
        [ setup 2768625435 [ 1 ]
        , setup 1007455905 [ 2 ]
        , fuzz fuzzBytes "crc32 output is always > 0" <|
            \buffer ->
                Crc32.crc32 buffer
                    |> Expect.atLeast 0
        ]
