module TestDeflate exposing (suite)

import Array exposing (Array)
import ByteArray
import Bytes
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Deflate.Internal as Deflate
import Deflate.Symbol as Symbol exposing (Symbol(..))
import Expect exposing (Expectation)
import Flate as Public
import Frankenstein
import Fuzz exposing (Fuzzer, int, list, string)
import Huffman
import Inflate.Inflate as Inflate
import LZ77
import LengthLimitedHuffmanCodes
import Test exposing (..)
import Text


share { length, distance } =
    Symbol.Share length distance


foo =
    Encode.encode (Encode.string "foo")


bar =
    Encode.encode (Encode.string "bar")


suite : Test
suite =
    describe "Deflate"
        [ describe "raw"
            [ test "foo" <|
                \_ ->
                    let
                        encoded =
                            Deflate.encodeRaw foo

                        asBytes =
                            Decode.decode (ByteArray.decoder (Bytes.width encoded)) encoded
                    in
                    asBytes
                        |> Expect.equal (Just (Array.fromList [ 1, 3, 0, 252, 255, 102, 111, 111 ]))
            , test "bar" <|
                \_ ->
                    let
                        encoded =
                            Deflate.encodeRaw bar

                        asBytes =
                            Decode.decode (ByteArray.decoder (Bytes.width encoded)) encoded
                    in
                    asBytes
                        |> Expect.equal (Just (Array.fromList [ 1, 3, 0, 252, 255, 98, 97, 114 ]))
            , test "frankenstein" <|
                \_ ->
                    Frankenstein.text
                        |> Encode.string
                        |> Encode.encode
                        |> Deflate.encodeRaw
                        |> Inflate.inflate
                        |> Maybe.andThen (Decode.decode (Decode.string (Encode.getStringWidth Frankenstein.text)))
                        |> Expect.equal (Just Frankenstein.text)
            ]
        , describe "static"
            [ test "foo" <|
                \_ ->
                    let
                        encoded =
                            Deflate.encodeStatic (Just 10) foo

                        asBytes =
                            Decode.decode (ByteArray.decoder (Bytes.width encoded)) encoded
                    in
                    asBytes
                        |> Expect.equal (Just (Array.fromList [ 75, 203, 207, 7, 0 ]))
            , test "bar" <|
                \_ ->
                    let
                        encoded =
                            Deflate.encodeStatic (Just 10) bar

                        asBytes =
                            Decode.decode (ByteArray.decoder (Bytes.width encoded)) encoded
                    in
                    asBytes
                        |> Expect.equal (Just (Array.fromList [ 75, 74, 44, 2, 0 ]))
            , test "frankenstein" <|
                \_ ->
                    Frankenstein.text
                        |> Encode.string
                        |> Encode.encode
                        |> Deflate.encodeStatic (Just LZ77.maxWindowSize)
                        |> Inflate.inflate
                        |> Maybe.andThen (Decode.decode (Decode.string (Encode.getStringWidth Frankenstein.text)))
                        |> Expect.equal (Just Frankenstein.text)
            ]
        , describe "dynamic"
            [ test "foo" <|
                \_ ->
                    let
                        encoded =
                            Deflate.encodeDynamic (Just 10) foo

                        asBytes =
                            Decode.decode (ByteArray.decoder (Bytes.width encoded)) encoded
                    in
                    asBytes
                        |> Expect.equal (Just (Array.fromList [ 5, 192, 33, 1, 0, 0, 0, 128, 160, 183, 86, 254, 55, 137, 1 ]))
            , test "bar" <|
                \_ ->
                    let
                        encoded =
                            Deflate.encodeDynamic (Just 10) bar

                        asBytes =
                            Decode.decode (ByteArray.decoder (Bytes.width encoded)) encoded
                    in
                    asBytes
                        |> Expect.equal (Just (Array.fromList [ 5, 192, 49, 1, 0, 0, 0, 64, 176, 172, 68, 208, 255, 48, 105 ]))
            , test "frankenstein" <|
                \_ ->
                    Frankenstein.text
                        |> Encode.string
                        |> Encode.encode
                        |> Deflate.encodeDynamic (Just LZ77.maxWindowSize)
                        |> Inflate.inflate
                        |> Maybe.andThen (Decode.decode (Decode.string (Encode.getStringWidth Frankenstein.text)))
                        |> Expect.equal (Just Frankenstein.text)
            ]
        , describe "gzip"
            [ test "frankenstein" <|
                \_ ->
                    Frankenstein.text
                        |> Encode.string
                        |> Encode.encode
                        |> Public.deflateGZip
                        |> Public.inflateGZip
                        |> Maybe.andThen (Decode.decode (Decode.string (Encode.getStringWidth Frankenstein.text)))
                        |> Expect.equal (Just Frankenstein.text)
            ]
        , describe "zlib"
            [ test "frankenstein" <|
                \_ ->
                    Text.text
                        |> Encode.string
                        |> Encode.encode
                        |> Public.deflateZlib
                        |> Public.inflateZlib
                        |> Maybe.andThen (Decode.decode (Decode.string (Encode.getStringWidth Text.text)))
                        |> Expect.equal (Just Text.text)
            ]
        , frankenstein11
        , dynamicHuffmanTableTest
        , bug1
        ]


frankenstein11 =
    describe "frankenstein first paragraph"
        [ test "raw" <|
            \_ ->
                Deflate.encodeRaw frankenstein11Bytes
                    |> Inflate.inflate
                    |> Maybe.andThen (Decode.decode (Decode.string (Encode.getStringWidth frankenstein11Text)))
                    |> Expect.equal (Just frankenstein11Text)
        , test "static" <|
            \_ ->
                Deflate.encodeStatic (Just 10) frankenstein11Bytes
                    |> Inflate.inflate
                    |> Maybe.andThen (Decode.decode (Decode.string (Encode.getStringWidth frankenstein11Text)))
                    |> Expect.equal (Just frankenstein11Text)
        ]


frankenstein11Bytes =
    Encode.encode (Encode.string frankenstein11Text)


frankenstein11Text =
    """I am by birth a Genevese, and my family is one of the most distinguished of that republic. My ancestors had been for many years counsellors and syndics, and my father had filled several public situations with honour and reputation. He was respected by all who knew him for his integrity and indefatigable attention to public business. He passed his younger days perpetually occupied by the affairs of his country; a variety of circumstances had prevented his marrying early, nor was it until the decline of life that he became a husband and the father of a family."""


dynamicHuffmanTableTest =
    describe "dynamic huffman table"
        [ test "foo" <|
            \_ ->
                let
                    compressed =
                        Array.fromList [ Literal 102, Literal 111, Literal 111, EndOfBlock ]

                    tree =
                        Symbol.buildDynamicHuffmanCodec compressed
                in
                tree
                    |> Expect.equal fooTable
        , test "find frequencies for foo" <|
            \_ ->
                let
                    symbols =
                        Array.fromList [ Literal 102, Literal 111, Literal 111, EndOfBlock ]

                    freqs =
                        Array.foldl Symbol.dynamicFindFrequencies ( Array.repeat 286 0, Array.repeat 30 0, True ) symbols
                in
                freqs
                    |> Expect.equal
                        ( Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                        , Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                        , True
                        )
        , test "length limited huffman codes length" <|
            \_ ->
                LengthLimitedHuffmanCodes.calculate 2
                    (Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ])
                    |> Expect.equal (Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ])
        , test "package" <|
            \_ ->
                LengthLimitedHuffmanCodes.package
                    (Array.fromList
                        [ { symbols = Array.fromList [ 102 ], weight = 1 }
                        , { symbols = Array.fromList [ 256 ], weight = 1 }
                        , { symbols = Array.fromList [ 111 ], weight = 2 }
                        ]
                    )
                    |> Expect.equal (Array.fromList [ { symbols = Array.fromList [ 102, 256 ], weight = 2 } ])
        , test "merge" <|
            \_ ->
                let
                    source =
                        Array.fromList
                            [ { symbols = Array.fromList [ 102 ], weight = 1 }
                            , { symbols = Array.fromList [ 256 ], weight = 1 }
                            , { symbols = Array.fromList [ 111 ], weight = 2 }
                            ]

                    packaged =
                        LengthLimitedHuffmanCodes.package source

                    expected =
                        Array.fromList
                            [ { symbols = Array.fromList [ 102 ], weight = 1 }
                            , { symbols = Array.fromList [ 256 ], weight = 1 }
                            , { symbols = Array.fromList [ 111 ], weight = 2 }
                            , { symbols = Array.fromList [ 102, 256 ], weight = 2 }
                            ]
                in
                LengthLimitedHuffmanCodes.merge packaged source
                    |> Expect.equal expected
        , test "restoreCanonicalHuffmanCodes literal" <|
            \_ ->
                let
                    bitWidths =
                        Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                in
                Huffman.restoreCanonicalHuffmanCodes bitWidths (Huffman.new 257)
                    |> Expect.equal fooTable.literal
        , test "restoreCanonicalHuffmanCodes distance" <|
            \_ ->
                let
                    bitWidths =
                        Array.fromList [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                in
                Huffman.restoreCanonicalHuffmanCodes bitWidths (Huffman.new 1)
                    |> Expect.equal fooTable.distance
        , test "calculateRunLengths" <|
            \_ ->
                let
                    literalCodeCount =
                        257

                    distanceCodeCount =
                        1

                    expected =
                        [ { value = 0, count = 102 }
                        , { value = 2, count = 1 }
                        , { value = 0, count = 8 }
                        , { value = 1, count = 1 }
                        , { value = 0, count = 144 }
                        , { value = 2, count = 1 }
                        , { value = 1, count = 1 }
                        ]
                in
                Symbol.calculateRunLengths [ ( fooTable.literal, literalCodeCount ), ( fooTable.distance, distanceCodeCount ) ] []
                    |> Expect.equal (Array.fromList expected)
        , test "calculateCodes" <|
            \_ ->
                let
                    input =
                        Array.fromList
                            [ { value = 0, count = 102 }
                            , { value = 2, count = 1 }
                            , { value = 0, count = 8 }
                            , { value = 1, count = 1 }
                            , { value = 0, count = 144 }
                            , { value = 2, count = 1 }
                            , { value = 1, count = 1 }
                            ]
                in
                Symbol.calculateCodes input
                    |> Expect.equal (Array.fromList [ ( 18, 7, 91 ), ( 2, 0, 0 ), ( 17, 3, 5 ), ( 1, 0, 0 ), ( 18, 7, 127 ), ( 17, 3, 3 ), ( 2, 0, 0 ), ( 1, 0, 0 ) ])
        ]


fooTable =
    { literal = Huffman.fromList [ { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 2, bits = 1 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 1, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 2, bits = 3 } ]
    , distance = Huffman.fromList [ { width = 1, bits = 0 } ]
    }


bug1 =
    describe "bug 1" <|
        let
            frequencies =
                Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 3, 5, 20, 3, 4, 8, 8, 0, 0, 6, 8, 12, 12, 3, 0, 11, 10, 10, 1, 4, 8, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 11, 7, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]

            huffmanTree =
                Huffman.fromList [ { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 3, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 6, bits = 27 }, { width = 8, bits = 31 }, { width = 7, bits = 15 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 159 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 95 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 223 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 7, bits = 79 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 63 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 4, bits = 4 }, { width = 8, bits = 191 }, { width = 6, bits = 59 }, { width = 5, bits = 9 }, { width = 4, bits = 12 }, { width = 6, bits = 7 }, { width = 6, bits = 39 }, { width = 5, bits = 25 }, { width = 5, bits = 5 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 5, bits = 21 }, { width = 5, bits = 13 }, { width = 4, bits = 2 }, { width = 4, bits = 10 }, { width = 6, bits = 23 }, { width = 0, bits = 0 }, { width = 4, bits = 6 }, { width = 5, bits = 29 }, { width = 4, bits = 14 }, { width = 8, bits = 127 }, { width = 6, bits = 55 }, { width = 5, bits = 3 }, { width = 0, bits = 0 }, { width = 5, bits = 19 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 255 }, { width = 4, bits = 1 }, { width = 5, bits = 11 }, { width = 7, bits = 47 }, { width = 0, bits = 0 }, { width = 7, bits = 111 } ]
        in
        [ {-
             , test "bitwidth" <|
                 \_ ->
                     let
                         trees =
                             { literal = Huffman.fromList [ { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 3, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 6, bits = 27 }, { width = 8, bits = 31 }, { width = 7, bits = 15 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 159 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 95 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 223 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 7, bits = 79 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 63 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 4, bits = 4 }, { width = 8, bits = 191 }, { width = 6, bits = 59 }, { width = 5, bits = 9 }, { width = 4, bits = 12 }, { width = 6, bits = 7 }, { width = 6, bits = 39 }, { width = 5, bits = 25 }, { width = 5, bits = 5 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 5, bits = 21 }, { width = 5, bits = 13 }, { width = 4, bits = 2 }, { width = 4, bits = 10 }, { width = 6, bits = 23 }, { width = 0, bits = 0 }, { width = 4, bits = 6 }, { width = 5, bits = 29 }, { width = 4, bits = 14 }, { width = 8, bits = 127 }, { width = 6, bits = 55 }, { width = 5, bits = 3 }, { width = 0, bits = 0 }, { width = 5, bits = 19 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 255 }, { width = 4, bits = 1 }, { width = 5, bits = 11 }, { width = 7, bits = 47 }, { width = 0, bits = 0 }, { width = 7, bits = 111 } ]
                             , distance = Huffman.fromList [ { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 4, bits = 3 }, { width = 3, bits = 0 }, { width = 4, bits = 11 }, { width = 3, bits = 4 }, { width = 4, bits = 7 }, { width = 3, bits = 2 }, { width = 3, bits = 6 }, { width = 3, bits = 1 }, { width = 3, bits = 5 }, { width = 4, bits = 15 } ]
                             }

                         literal_code_count =
                             262

                         distance_code_count =
                             1
                     in
                     Symbol.buildBitWidthCodes literal_code_count distance_code_count { literal = trees.literal, distance = trees.distance }
                         |> Expect.equal (Array.fromList [ ( 18, 7, 21 ), ( 3, 0, 0 ), ( 18, 7, 0 ), ( 6, 0, 0 ), ( 8, 0, 0 ), ( 7, 0, 0 ), ( 18, 7, 1 ), ( 8, 0, 0 ), ( 17, 3, 2 ), ( 8, 0, 0 ), ( 17, 3, 0 ), ( 8, 0, 0 ), ( 17, 3, 0 ), ( 7, 0, 0 ), ( 17, 3, 7 ), ( 8, 0, 0 ), ( 18, 7, 1 ), ( 4, 0, 0 ), ( 8, 0, 0 ), ( 6, 0, 0 ), ( 5, 0, 0 ), ( 4, 0, 0 ), ( 6, 0, 0 ), ( 6, 0, 0 ), ( 5, 0, 0 ), ( 5, 0, 0 ), ( 0, 0, 0 ), ( 0, 0, 0 ), ( 5, 0, 0 ), ( 5, 0, 0 ), ( 4, 0, 0 ), ( 4, 0, 0 ), ( 6, 0, 0 ), ( 0, 0, 0 ), ( 4, 0, 0 ), ( 5, 0, 0 ), ( 4, 0, 0 ), ( 8, 0, 0 ), ( 6, 0, 0 ), ( 5, 0, 0 ), ( 0, 0, 0 ), ( 5, 0, 0 ), ( 18, 7, 123 ), ( 8, 0, 0 ), ( 4, 0, 0 ), ( 5, 0, 0 ), ( 7, 0, 0 ), ( 0, 0, 0 ), ( 7, 0, 0 ), ( 17, 3, 4 ), ( 4, 0, 0 ), ( 3, 0, 0 ), ( 4, 0, 0 ), ( 3, 0, 0 ), ( 4, 0, 0 ), ( 3, 0, 0 ), ( 16, 2, 0 ), ( 4, 0, 0 ) ])
          -}
          test "frankeinstein lz77 conversion" <|
            \_ ->
                let
                    expected =
                        [ Literal 119, Literal 114, Literal 105, Literal 116, Literal 101, Literal 32, Literal 97, Literal 32, Literal 102, Literal 101, Literal 119, Literal 32, Literal 108, Literal 105, Literal 110, Literal 101, Literal 115, Literal 32, Literal 105, Literal 110, Literal 32, Literal 104, Literal 97, Literal 115, share { length = 3, distance = 21 }, Literal 116, Literal 111, Literal 32, Literal 115, Literal 97, Literal 121, Literal 32, Literal 116, Literal 104, Literal 97, Literal 116, Literal 32, Literal 73, Literal 32, Literal 97, Literal 109, share { length = 3, distance = 14 }, Literal 102, Literal 101, Literal 45, Literal 97, Literal 110, Literal 100, Literal 32, Literal 119, Literal 101, Literal 108, Literal 108, Literal 32, Literal 97, Literal 100, Literal 118, Literal 97, Literal 110, Literal 99, Literal 101, Literal 100, Literal 32, Literal 111, Literal 110, Literal 32, Literal 109, Literal 121, Literal 32, Literal 118, Literal 111, Literal 121, Literal 97, Literal 103, Literal 101, Literal 46, Literal 32, Literal 84, Literal 104, Literal 105, Literal 115, Literal 32, Literal 108, Literal 101, Literal 116, Literal 116, Literal 101, Literal 114, Literal 32, Literal 119, Literal 105, share { length = 3, distance = 40 }, Literal 114, Literal 101, Literal 97, Literal 99, Literal 104, Literal 32, Literal 69, Literal 110, Literal 103, Literal 108, share { length = 4, distance = 59 }, Literal 98, Literal 121, share { length = 3, distance = 109 }, Literal 109, Literal 101, Literal 114, Literal 99, Literal 104, Literal 97, Literal 110, Literal 116, Literal 109, Literal 97, Literal 110, Literal 32, Literal 110, Literal 111, Literal 119, share { length = 4, distance = 66 }, Literal 105, Literal 116, Literal 115, Literal 32, Literal 104, Literal 111, Literal 109, Literal 101, Literal 119, Literal 97, Literal 114, Literal 100, share { length = 7, distance = 76 }, Literal 32, Literal 102, Literal 114, Literal 111, Literal 109, Literal 32, Literal 65, share { length = 5, distance = 43 }, Literal 103, Literal 101, Literal 108, Literal 59, Literal 32, Literal 109, Literal 111, Literal 114, share { length = 3, distance = 21 }, Literal 111, Literal 114, Literal 116, Literal 117, Literal 110, Literal 97, share { length = 4, distance = 160 }, share { length = 3, distance = 24 }, Literal 32, Literal 73, Literal 44, Literal 32, Literal 119, Literal 104, Literal 111, Literal 32, Literal 109, share { length = 3, distance = 169 }, Literal 110, Literal 111, Literal 116, Literal 32, Literal 115, Literal 101, Literal 101, share { length = 4, distance = 141 }, share { length = 3, distance = 32 }, Literal 105, Literal 118, Literal 101, Literal 32, share { length = 4, distance = 114 }, Literal 44, Literal 32, Literal 112, Literal 101, Literal 114, Literal 104, Literal 97, Literal 112, Literal 115, Literal 44, share { length = 4, distance = 59 }, share { length = 3, distance = 41 }, Literal 110, Literal 121, Literal 32, Literal 121, Literal 101, Literal 97, Literal 114, Literal 115, Literal 46, share { length = 5, distance = 213 }, Literal 44, share { length = 3, distance = 118 }, Literal 119, Literal 101, Literal 118, Literal 101, Literal 114, Literal 44, share { length = 4, distance = 249 }, Literal 103, Literal 111, Literal 111, Literal 100, Literal 32, Literal 115, Literal 112, Literal 105, share { length = 3, distance = 277 }, Literal 115, EndOfBlock ]
                in
                Text.text
                    |> Encode.string
                    |> Encode.encode
                    |> Deflate.compress (Just LZ77.maxWindowSize)
                    |> Expect.equal (Array.fromList expected)
        , test "correct huffman tree built?" <|
            \_ ->
                let
                    expected =
                        { literal = Huffman.fromList [ { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 3, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 6, bits = 27 }, { width = 8, bits = 31 }, { width = 7, bits = 15 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 159 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 95 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 223 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 7, bits = 79 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 63 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 4, bits = 4 }, { width = 8, bits = 191 }, { width = 6, bits = 59 }, { width = 5, bits = 9 }, { width = 4, bits = 12 }, { width = 6, bits = 7 }, { width = 6, bits = 39 }, { width = 5, bits = 25 }, { width = 5, bits = 5 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 5, bits = 21 }, { width = 5, bits = 13 }, { width = 4, bits = 2 }, { width = 4, bits = 10 }, { width = 6, bits = 23 }, { width = 0, bits = 0 }, { width = 4, bits = 6 }, { width = 5, bits = 29 }, { width = 4, bits = 14 }, { width = 8, bits = 127 }, { width = 6, bits = 55 }, { width = 5, bits = 3 }, { width = 0, bits = 0 }, { width = 5, bits = 19 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 8, bits = 255 }, { width = 4, bits = 1 }, { width = 5, bits = 11 }, { width = 7, bits = 47 }, { width = 0, bits = 0 }, { width = 7, bits = 111 } ]
                        , distance = Huffman.fromList [ { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 0, bits = 0 }, { width = 4, bits = 3 }, { width = 3, bits = 0 }, { width = 4, bits = 11 }, { width = 3, bits = 4 }, { width = 4, bits = 7 }, { width = 3, bits = 2 }, { width = 3, bits = 6 }, { width = 3, bits = 1 }, { width = 3, bits = 5 }, { width = 4, bits = 15 } ]
                        }
                in
                Text.text
                    |> Encode.string
                    |> Encode.encode
                    |> Deflate.compress (Just LZ77.maxWindowSize)
                    |> Symbol.buildDynamicHuffmanCodec
                    |> Expect.equal expected
        , test "from frequencies" <|
            \_ ->
                Huffman.fromFrequencies frequencies 15
                    |> Expect.equal huffmanTree
        , test "calculate" <|
            \_ ->
                let
                    maxBitWidth =
                        8

                    expected =
                        Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 8, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 8, 0, 0, 0, 8, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 8, 6, 5, 4, 6, 6, 5, 5, 0, 0, 5, 5, 4, 4, 6, 0, 4, 5, 4, 8, 6, 5, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 4, 5, 7, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                in
                LengthLimitedHuffmanCodes.calculate maxBitWidth frequencies
                    |> Expect.equal expected
        ]
