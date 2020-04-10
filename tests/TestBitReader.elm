module TestBitReader exposing (suite)

import ByteArray
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Experimental.BitReader as BitReader
import Experimental.Inflate as Inflate
import Flate
import LZ77
import Test exposing (..)


suite =
    describe "BitReader"
        [ test "bytes" <|
            \_ ->
                let
                    decoder =
                        BitReader.decode (BitReader.exactly 10 (BitReader.readBits 8))
                in
                List.range 0 9
                    |> List.map Encode.unsignedInt8
                    |> Encode.sequence
                    |> Encode.encode
                    |> decoder
                    |> Expect.equal (Ok (List.range 0 9))
        , test "ones as uint8" <|
            \_ ->
                let
                    decoder =
                        BitReader.decode (BitReader.exactly 10 (BitReader.readBits 8))
                in
                List.repeat 10 0xFF
                    |> List.map Encode.unsignedInt8
                    |> Encode.sequence
                    |> Encode.encode
                    |> decoder
                    |> Expect.equal (Ok (List.repeat 10 0xFF))
        , test "ones as uint16" <|
            \_ ->
                let
                    decoder =
                        BitReader.decode (BitReader.exactly 5 (BitReader.readBits 16))
                in
                List.repeat 10 0xFF
                    |> List.map Encode.unsignedInt8
                    |> Encode.sequence
                    |> Encode.encode
                    |> decoder
                    |> Expect.equal (Ok (List.repeat 5 0xFFFF))
        , test "unsignedInt16 LE" <|
            \_ ->
                let
                    decoder =
                        BitReader.decode (BitReader.exactly 10 (BitReader.unsignedInt16 LE))
                in
                List.repeat 10 0xABCD
                    |> List.map (Encode.unsignedInt16 LE)
                    |> Encode.sequence
                    |> Encode.encode
                    |> decoder
                    |> Expect.equal (Ok (List.repeat 10 0xABCD))
        , test "unsignedInt16 BE" <|
            \_ ->
                let
                    decoder =
                        BitReader.decode (BitReader.exactly 10 (BitReader.unsignedInt16 BE))
                in
                List.repeat 10 0xABCD
                    |> List.map (Encode.unsignedInt16 BE)
                    |> Encode.sequence
                    |> Encode.encode
                    |> decoder
                    |> Expect.equal (Ok (List.repeat 10 0xABCD))
        , test "map2" <|
            \_ ->
                let
                    decoder =
                        BitReader.decode (BitReader.exactly 5 (BitReader.map2 (\a b -> a + b) (BitReader.unsignedInt16 LE) (BitReader.unsignedInt16 LE)))
                in
                List.repeat 10 0xABCD
                    |> List.map (Encode.unsignedInt16 LE)
                    |> Encode.sequence
                    |> Encode.encode
                    |> decoder
                    |> Expect.equal (Ok (List.repeat 5 (0xABCD * 2)))
        , inflate
        ]


inflate =
    describe "Inflate"
        [ describe "uncompressed"
            [ test "foo" <|
                \_ ->
                    let
                        bytes =
                            "foo"
                                |> Encode.string
                                |> Encode.encode
                                |> Flate.deflateWithOptions Flate.Raw
                                |> (\buffer -> Decode.decode (ByteArray.decoder (Bytes.width buffer)) buffer)
                    in
                    "foo"
                        |> Encode.string
                        |> Encode.encode
                        |> Flate.deflateWithOptions Flate.Raw
                        |> Inflate.inflate
                        |> Result.toMaybe
                        |> Maybe.andThen (Decode.decode (Decode.string 3))
                        |> Expect.equal (Just "foo")
            ]
        , describe "static"
            [ test "foo" <|
                \_ ->
                    let
                        bytes =
                            "foo"
                                |> Encode.string
                                |> Encode.encode
                                |> Flate.deflateWithOptions (Flate.Static (Flate.WithWindowSize LZ77.maxWindowSize))
                                |> (\buffer -> Decode.decode (ByteArray.decoder (Bytes.width buffer)) buffer)
                    in
                    "foo"
                        |> Encode.string
                        |> Encode.encode
                        |> Flate.deflateWithOptions (Flate.Static (Flate.WithWindowSize LZ77.maxWindowSize))
                        |> Inflate.inflate
                        |> Result.toMaybe
                        |> Maybe.andThen (Decode.decode (Decode.string 3))
                        |> Expect.equal (Just "foo")
            ]
        , describe "dynamic"
            [ test "foo" <|
                \_ ->
                    let
                        bytes =
                            "foo"
                                |> Encode.string
                                |> Encode.encode
                                |> Flate.deflateWithOptions (Flate.Dynamic (Flate.WithWindowSize LZ77.maxWindowSize))
                                |> (\buffer -> Decode.decode (ByteArray.decoder (Bytes.width buffer)) buffer)
                    in
                    "foo"
                        |> Encode.string
                        |> Encode.encode
                        |> Flate.deflateWithOptions (Flate.Dynamic (Flate.WithWindowSize LZ77.maxWindowSize))
                        |> Inflate.inflate
                        |> Result.toMaybe
                        |> Maybe.andThen (Decode.decode (Decode.string 3))
                        |> Expect.equal (Just "foo")
            ]
        ]
