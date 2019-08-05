module TestInflate exposing (suite)

import Array exposing (Array)
import Bitwise
import ByteArray
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Flate as External
import Fuzz exposing (Fuzzer, int, list, string)
import Inflate.GZip as GZip
import Inflate.Internal as Inflate
import Inflate.ZLib as ZLib
import Test exposing (..)
import TestData.Havamal as Havamal
import TestData.Lorem as Lorem


mySuite =
    let
        setup index input output =
            test ("lot of zeros " ++ String.fromInt index) <|
                \_ ->
                    input
                        |> Array.fromList
                        |> ByteArray.toBytes
                        |> External.inflate
                        |> Maybe.andThen (\b -> Decode.decode (exactly (Bytes.width b) Decode.unsignedInt8) b)
                        |> Expect.equal (Just output)
    in
    setup 12 [ 5, 192, 177, 16, 0, 0, 12, 4, 193, 45, 2, 16, 165, 247, 151, 186, 193, 253, 16 ] [ 0, 0, 0, 4, 16, 65, 0, 0, 0 ]


exactly n decoder =
    let
        go ( i, accum ) =
            if i < n then
                decoder
                    |> Decode.map (\v -> Decode.Loop ( i + 1, v :: accum ))

            else
                Decode.succeed (Decode.Done (List.reverse accum))
    in
    Decode.loop ( 0, [] ) go


suite =
    describe "tiny inflate"
        [ buildBits
        , buildSymbolTree
        , bitShifts

        -- , havamal
        -- , lorem
        , example
        , various
        , buildTable
        ]


various =
    describe "various"
        [ describe "lots of zeros" <|
            let
                setup index input output =
                    test ("lot of zeros " ++ String.fromInt index) <|
                        \_ ->
                            input
                                |> Array.fromList
                                |> ByteArray.toBytes
                                |> External.inflate
                                |> Maybe.andThen (\b -> Decode.decode (exactly (Bytes.width b) Decode.unsignedInt8) b)
                                |> Expect.equal (Just output)
            in
            [ setup 0 [ 5, 192, 1, 9, 0, 0, 0, 0, 16, 255, 87, 11, 1 ] [ 0, 0, 0 ]
            , setup 1 [ 5, 192, 177, 16, 0, 0, 12, 4, 193, 45, 2, 16, 165, 247, 151, 186, 193, 253, 2 ] [ 0, 0, 0, 4, 16, 65 ]
            , setup 2 [ 0x5D, 0xC8, 0x21, 0x01, 0x00, 0x00, 0x00, 0xC3, 0x20, 0x82, 0xBC, 0x7F, 0xCE, 0xFB, 0x21, 0x11, 0x6B, 0xE0 ] [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
            , setup 3 [ 5, 192, 129, 12, 0, 0, 0, 128, 176, 131, 228, 207, 217, 132, 1 ] [ 0, 28, 0, 0, 0 ]
            , setup 4 [ 85, 199, 33, 1, 0, 0, 0, 194, 48, 130, 208, 63, 231, 5, 134, 207, 45, 175, 218, 0 ] [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
            , setup 6 [ 21, 196, 49, 1, 0, 0, 0, 131, 32, 131, 172, 127, 78, 29, 7, 100, 15, 1 ] [ 0, 0, 0, 0, 28, 0, 0, 0, 0, 0 ]
            , setup 8 [ 5, 128, 129, 12, 0, 0, 0, 131, 142, 144, 218, 153, 18, 111, 67, 30 ] [ 0, 16, 131, 16, 81 ]
            , setup 10 [ 5, 128, 1, 9, 0, 0, 8, 195, 38, 24, 96, 145, 172, 96, 35, 193, 226, 7, 223, 61, 28, 168, 54 ] [ 0, 16, 131, 16, 81, 128, 0, 16, 64, 0, 0, 1, 4, 16 ]
            , setup 12 [ 5, 192, 177, 16, 0, 0, 12, 4, 193, 45, 2, 16, 165, 247, 151, 186, 193, 253, 16 ] [ 0, 0, 0, 4, 16, 65, 0, 0, 0 ]
            ]
        , describe "lots of zeros zlib" <|
            let
                setup index input output =
                    test ("lot of zeros " ++ String.fromInt index) <|
                        \_ ->
                            case input |> Array.fromList |> ByteArray.toBytes |> ZLib.inflate of
                                Ok v ->
                                    Just v
                                        |> Maybe.andThen (\b -> Decode.decode (exactly (Bytes.width b) Decode.unsignedInt8) b)
                                        |> Expect.equal (Just output)

                                Err e ->
                                    Expect.fail (Debug.toString e)
            in
            [ setup 0
                [ 0x78, 0x9C, 0x63, 0x60, 0xA0, 0x1C, 0xC8, 0xE0, 0xC0, 0xA3, 0x60, 0x14, 0x8C, 0x82, 0xE1, 0x0D, 0x00, 0x9B, 0x2E, 0x00, 0xA9 ]
                ([ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28 ] ++ List.repeat 999 0)
            ]
        , test "foo dynamic" <|
            \_ ->
                [ 0x05, 0xC0, 0x21, 0x0D, 0x00, 0x00, 0x00, 0x80, 0xB0, 0xB6, 0xD8, 0xF7, 0x77, 0x2C, 0x06 ]
                    |> Array.fromList
                    |> ByteArray.toBytes
                    |> External.inflate
                    |> Maybe.andThen (Decode.decode (Decode.string 3))
                    |> Expect.equal (Just "foo")
        , test "foo static" <|
            \_ ->
                [ 0x4B, 0xCB, 0xCF, 0x07, 0x00 ]
                    |> Array.fromList
                    |> ByteArray.toBytes
                    |> External.inflate
                    |> Maybe.andThen (Decode.decode (Decode.string 3))
                    |> Expect.equal (Just "foo")
        , test "foo none" <|
            \_ ->
                [ 0x01, 0x03, 0x00, 0xFC, 0xFF, 0x66, 0x6F, 0x6F ]
                    |> Array.fromList
                    |> ByteArray.toBytes
                    |> External.inflate
                    |> Maybe.andThen (Decode.decode (Decode.string 3))
                    |> Expect.equal (Just "foo")
        , describe "zlib" <|
            let
                setup name input output =
                    test name <|
                        \_ ->
                            case input |> Array.fromList |> ByteArray.toBytes |> ZLib.inflate of
                                Ok v ->
                                    Just v
                                        |> Maybe.andThen (\b -> Decode.decode (Decode.string 3) b)
                                        |> Expect.equal (Just output)

                                Err e ->
                                    Expect.fail (Debug.toString e)
            in
            [ setup "zlib foo none" [ 0x78, 0x01, 0x01, 0x03, 0x00, 0xFC, 0xFF, 0x66, 0x6F, 0x6F, 0x02, 0x82, 0x01, 0x45 ] "foo"
            , setup "zlib foo fixed" [ 0x78, 0x5E, 0x4B, 0xCB, 0xCF, 0x07, 0x00, 0x02, 0x82, 0x01, 0x45 ] "foo"
            , setup "zlib foo dynamic" [ 0x78, 0x9C, 0x05, 0xC0, 0x21, 0x0D, 0x00, 0x00, 0x00, 0x80, 0xB0, 0xB6, 0xD8, 0xF7, 0x77, 0x2C, 0x06, 0x02, 0x82, 0x01, 0x45 ] "foo"
            ]

        {-
           , describe "gzip foo" <|
               let
                   setup name hexes =
                       test name <|
                           \_ ->
                               hexes
                                   |> Array.fromList
                                   |> ByteArray.toBytes
                                   |> GZip.inflate
                                   |> Maybe.andThen (Decode.decode (Decode.string 3))
                                   |> Expect.equal (Just "foo")
               in
               [ setup "dynamic" [ 0x1F, 0x8B, 0x08, 0x00, 0xED, 0xC1, 0x1B, 0x5D, 0x00, 0xFF, 0x05, 0xC0, 0x21, 0x0D, 0x00, 0x00, 0x00, 0x80, 0xB0, 0xB6, 0xD8, 0xF7, 0x77, 0x2C, 0x06, 0x21, 0x65, 0x73, 0x8C, 0x03, 0x00, 0x00, 0x00 ]
               , setup "dynamic with fname" [ 0x1F, 0x8B, 0x08, 0x08, 0x5F, 0xCA, 0x1B, 0x5D, 0x00, 0xFF, 0x66, 0x6F, 0x6F, 0x2E, 0x74, 0x78, 0x74, 0x00, 0x05, 0xC0, 0x21, 0x0D, 0x00, 0x00, 0x00, 0x80, 0xB0, 0xB6, 0xD8, 0xF7, 0x77, 0x2C, 0x06, 0x21, 0x65, 0x73, 0x8C, 0x03, 0x00, 0x00, 0x00 ]
               , setup "dynamic with fname and fcomment" [ 0x1F, 0x8B, 0x08, 0x08, 0x8D, 0xCA, 0x1B, 0x5D, 0x00, 0xFF, 0x66, 0x6F, 0x6F, 0x2E, 0x74, 0x78, 0x74, 0x00, 0x05, 0xC0, 0x21, 0x0D, 0x00, 0x00, 0x00, 0x80, 0xB0, 0xB6, 0xD8, 0xF7, 0x77, 0x2C, 0x06, 0x21, 0x65, 0x73, 0x8C, 0x03, 0x00, 0x00, 0x00 ]
               , setup "dynamic with fname and fcomment and checksum" [ 0x1F, 0x8B, 0x08, 0x0A, 0xB5, 0x98, 0x1C, 0x5D, 0x00, 0xFF, 0x62, 0x61, 0x72, 0x00, 0xBC, 0x0B, 0x05, 0xC0, 0x21, 0x0D, 0x00, 0x00, 0x00, 0x80, 0xB0, 0xB6, 0xD8, 0xF7, 0x77, 0x2C, 0x06, 0x21, 0x65, 0x73, 0x8C, 0x03, 0x00, 0x00, 0x00 ]
               , setup "dynamic with checksum" [ 0x1F, 0x8B, 0x08, 0x02, 0x62, 0xD8, 0x1B, 0x5D, 0x00, 0xFF, 0x24, 0x3E, 0x05, 0xC0, 0x21, 0x0D, 0x00, 0x00, 0x00, 0x80, 0xB0, 0xB6, 0xD8, 0xF7, 0x77, 0x2C, 0x06, 0x21, 0x65, 0x73, 0x8C, 0x03, 0x00, 0x00, 0x00 ]
               ]
        -}
        ]


example =
    let
        deyrfé : String
        deyrfé =
            "Deyr fé, deyja frændr"

        -- the `havamal` string compressed with zlib
        compressed : Bytes.Bytes
        compressed =
            [ 115, 73, 173, 44, 82, 72, 59, 188, 82, 71, 33, 37, 181, 50, 43, 81, 33, 173, 232, 240, 178, 188, 148, 34, 46, 0 ]
                |> List.map Encode.unsignedInt8
                |> Encode.sequence
                |> Encode.encode

        decompressedLength : Int
        decompressedLength =
            -- + 2 because string uses 2 non-ascii characters
            -- (they take an extra byte)
            String.length deyrfé + 2

        decode : Bytes -> Maybe String
        decode =
            Decode.decode (Decode.string decompressedLength)

        decompressed : String
        decompressed =
            External.inflate compressed
                |> Maybe.andThen decode
                |> Maybe.withDefault ""
    in
    test "example" <|
        \_ ->
            decompressed
                |> Expect.equal deyrfé


shiftOntoTag tag value bitcount =
    Bitwise.or tag (Bitwise.shiftLeftBy bitcount value)


shiftOffTag tag numberOfBits =
    Bitwise.and tag (Bitwise.shiftRightZfBy (16 - numberOfBits) 0xFFFF)


shiftOffTag_ tag numberOfBits =
    let
        val =
            Bitwise.and tag (Bitwise.shiftRightZfBy (16 - numberOfBits) 0xFFFF)

        newTag =
            Bitwise.shiftRightZfBy numberOfBits tag
    in
    ( val, newTag )


bitShifts =
    describe "bit shifts"
        [ describe "shift onto"
            [ test "bit shifts 10 " <| \_ -> shiftOntoTag 10 203 7 |> Expect.equal 25994
            , test "bit shifts 25994 " <| \_ -> shiftOntoTag 25994 177 15 |> Expect.equal 5825930
            , test "bit shifts 5825930 " <| \_ -> shiftOntoTag 5825930 9 23 |> Expect.equal 81323402
            , test "bit shifts 19854 " <| \_ -> shiftOntoTag 19854 128 19 |> Expect.equal 67128718
            , test "bit shifts 4195544 " <| \_ -> shiftOntoTag 4195544 48 23 |> Expect.equal 406848728
            , test "bit shifts 794626 " <| \_ -> shiftOntoTag 794626 16 22 |> Expect.equal 67903490
            , test "bit shifts 132624 " <| \_ -> shiftOntoTag 132624 70 21 |> Expect.equal 146933264
            , test "bit shifts 2295832 " <| \_ -> shiftOntoTag 2295832 225 23 |> Expect.equal 1889732632
            , test "bit shifts 3690884 " <| \_ -> shiftOntoTag 3690884 62 22 |> Expect.equal 263737732
            , test "bit shifts 515112 " <| \_ -> shiftOntoTag 515112 83 21 |> Expect.equal 174578728
            , test "bit shifts 2727792 " <| \_ -> shiftOntoTag 2727792 252 23 |> Expect.equal 2116657008
            , test "bit shifts 4134095 " <| \_ -> shiftOntoTag 4134095 3 22 |> Expect.equal 16717007
            , test "bit shifts 32650 " <| \_ -> shiftOntoTag 32650 4 21 |> Expect.equal 8421258
            , test "bit shifts 57472 " <| \_ -> shiftOntoTag 57472 176 21 |> Expect.equal 369156224
            , test "bit shifts 140472 " <| \_ -> shiftOntoTag 140472 92 21 |> Expect.equal 193078456
            , test "bit shifts 1881393 " <| \_ -> shiftOntoTag 1881393 227 21 |> Expect.equal 477934897
            , test "bit shifts 1866876 " <| \_ -> shiftOntoTag 1866876 45 21 |> Expect.equal 96238716
            , test "bit shifts 3494985 " <| \_ -> shiftOntoTag 3494985 131 22 |> Expect.equal 552948809
            ]
        , describe "shift off"
            [ test "bit shift off tag 81323402 " <| \_ -> shiftOffTag 81323402 2 |> Expect.equal 2
            , test "bit shift off tag 20330850 " <| \_ -> shiftOffTag 20330850 5 |> Expect.equal 2
            , test "bit shift off tag 635339 " <| \_ -> shiftOffTag 635339 5 |> Expect.equal 11
            , test "bit shift off tag 67128718 " <| \_ -> shiftOffTag 67128718 4 |> Expect.equal 14
            , test "bit shift off tag 406848728 " <| \_ -> shiftOffTag 406848728 3 |> Expect.equal 0
            , test "bit shift off tag 50856091 " <| \_ -> shiftOffTag 50856091 3 |> Expect.equal 3
            , test "bit shift off tag 6357011 " <| \_ -> shiftOffTag 6357011 3 |> Expect.equal 3
            , test "bit shift off tag 67903490 " <| \_ -> shiftOffTag 67903490 3 |> Expect.equal 2
            , test "bit shift off tag 8487936 " <| \_ -> shiftOffTag 8487936 3 |> Expect.equal 0
            , test "bit shift off tag 1060992 " <| \_ -> shiftOffTag 1060992 3 |> Expect.equal 0
            , test "bit shift off tag 146933264 " <| \_ -> shiftOffTag 146933264 3 |> Expect.equal 0
            , test "bit shift off tag 18366658 " <| \_ -> shiftOffTag 18366658 3 |> Expect.equal 2
            , test "bit shift off tag 1889732632 " <| \_ -> shiftOffTag 1889732632 3 |> Expect.equal 0
            , test "bit shift off tag 236216579 " <| \_ -> shiftOffTag 236216579 3 |> Expect.equal 3
            , test "bit shift off tag 29527072 " <| \_ -> shiftOffTag 29527072 3 |> Expect.equal 0
            , test "bit shift off tag 263737732 " <| \_ -> shiftOffTag 263737732 3 |> Expect.equal 4
            , test "bit shift off tag 32967216 " <| \_ -> shiftOffTag 32967216 3 |> Expect.equal 0
            , test "bit shift off tag 4120902 " <| \_ -> shiftOffTag 4120902 3 |> Expect.equal 6
            , test "bit shift off tag 174578728 " <| \_ -> shiftOffTag 174578728 3 |> Expect.equal 0
            , test "bit shift off tag 21822341 " <| \_ -> shiftOffTag 21822341 3 |> Expect.equal 5
            , test "bit shift off tag 2116657008 " <| \_ -> shiftOffTag 2116657008 3 |> Expect.equal 0
            , test "bit shift off tag 264582126 " <| \_ -> shiftOffTag 264582126 3 |> Expect.equal 6
            , test "bit shift off tag 16717007 " <| \_ -> shiftOffTag 16717007 3 |> Expect.equal 7
            , test "bit shift off tag 8421258 " <| \_ -> shiftOffTag 8421258 7 |> Expect.equal 10
            , test "bit shift off tag 369156224 " <| \_ -> shiftOffTag 369156224 7 |> Expect.equal 0
            , test "bit shift off tag 152529664 " <| \_ -> shiftOffTag 152529664 7 |> Expect.equal 0
            , test "bit shift off tag 8622454 " <| \_ -> shiftOffTag 8622454 3 |> Expect.equal 6
            , test "bit shift off tag 12288913 " <| \_ -> shiftOffTag 12288913 7 |> Expect.equal 17
            , test "bit shift off tag 193078456 " <| \_ -> shiftOffTag 193078456 3 |> Expect.equal 0
            , test "bit shift off tag 5161249 " <| \_ -> shiftOffTag 5161249 7 |> Expect.equal 33
            , test "bit shift off tag 2169875 " <| \_ -> shiftOffTag 2169875 3 |> Expect.equal 3
            , test "bit shift off tag 44444804 " <| \_ -> shiftOffTag 44444804 7 |> Expect.equal 4
            , test "bit shift off tag 477934897 " <| \_ -> shiftOffTag 477934897 7 |> Expect.equal 49
            , test "bit shift off tag 96238716 " <| \_ -> shiftOffTag 96238716 3 |> Expect.equal 4
            , test "bit shift off tag 7506344 " <| \_ -> shiftOffTag 7506344 3 |> Expect.equal 0
            , test "bit shift off tag 552948809 " <| \_ -> shiftOffTag 552948809 2 |> Expect.equal 1
            , test "bit shift off tag 300135275 " <| \_ -> shiftOffTag 300135275 4 |> Expect.equal 11
            ]
        , describe "shift off and update tag" <|
            [ test "shift off 81323402 " <| \_ -> shiftOffTag_ 81323402 2 |> Expect.equal (Tuple.pair 2 20330850)
            , test "shift off 20330850 " <| \_ -> shiftOffTag_ 20330850 5 |> Expect.equal (Tuple.pair 2 635339)
            , test "shift off 635339 " <| \_ -> shiftOffTag_ 635339 5 |> Expect.equal (Tuple.pair 11 19854)
            , test "shift off 67128718 " <| \_ -> shiftOffTag_ 67128718 4 |> Expect.equal (Tuple.pair 14 4195544)
            , test "shift off 406848728 " <| \_ -> shiftOffTag_ 406848728 3 |> Expect.equal (Tuple.pair 0 50856091)
            , test "shift off 50856091 " <| \_ -> shiftOffTag_ 50856091 3 |> Expect.equal (Tuple.pair 3 6357011)
            , test "shift off 6357011 " <| \_ -> shiftOffTag_ 6357011 3 |> Expect.equal (Tuple.pair 3 794626)
            , test "shift off 67903490 " <| \_ -> shiftOffTag_ 67903490 3 |> Expect.equal (Tuple.pair 2 8487936)
            , test "shift off 8487936 " <| \_ -> shiftOffTag_ 8487936 3 |> Expect.equal (Tuple.pair 0 1060992)
            , test "shift off 1060992 " <| \_ -> shiftOffTag_ 1060992 3 |> Expect.equal (Tuple.pair 0 132624)
            , test "shift off 146933264 " <| \_ -> shiftOffTag_ 146933264 3 |> Expect.equal (Tuple.pair 0 18366658)
            , test "shift off 18366658 " <| \_ -> shiftOffTag_ 18366658 3 |> Expect.equal (Tuple.pair 2 2295832)
            , test "shift off 1889732632 " <| \_ -> shiftOffTag_ 1889732632 3 |> Expect.equal (Tuple.pair 0 236216579)
            , test "shift off 236216579 " <| \_ -> shiftOffTag_ 236216579 3 |> Expect.equal (Tuple.pair 3 29527072)
            , test "shift off 29527072 " <| \_ -> shiftOffTag_ 29527072 3 |> Expect.equal (Tuple.pair 0 3690884)
            , test "shift off 263737732 " <| \_ -> shiftOffTag_ 263737732 3 |> Expect.equal (Tuple.pair 4 32967216)
            , test "shift off 32967216 " <| \_ -> shiftOffTag_ 32967216 3 |> Expect.equal (Tuple.pair 0 4120902)
            , test "shift off 4120902 " <| \_ -> shiftOffTag_ 4120902 3 |> Expect.equal (Tuple.pair 6 515112)
            ]
        ]


buildBits =
    describe "build bits "
        [ test "build bits I" <|
            \_ ->
                Inflate.buildBitsBase 4 3
                    |> Inflate.huffmanTableToList
                    |> List.map .base
                    |> Expect.equal
                        [ 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 259, 323 ]
        , test "build bits II" <|
            \_ ->
                Inflate.buildBitsBase 2 1
                    |> Inflate.huffmanTableToList
                    |> List.map .base
                    |> Expect.equal
                        [ 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577 ]
        ]


buildSymbolTree =
    let
        translation =
            [ 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 280, 281, 282, 283, 284, 285, 286, 287, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255 ]

        distance =
            [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    in
    describe "build symbol tree"
        [ test "build symbol tree I" <|
            \_ ->
                Inflate.sltree
                    |> .trans
                    |> Array.toList
                    |> Expect.equal translation
        , test "build distance tree I" <|
            \_ ->
                Inflate.sdtree
                    |> .trans
                    |> Array.toList
                    |> Expect.equal distance
        ]


havamal =
    let
        encode v =
            v |> List.map Encode.unsignedInt8 |> Encode.sequence |> Encode.encode

        decode b =
            Decode.decode (ByteArray.decoder (Bytes.width b)) b
                |> Maybe.map Array.toList
    in
    describe "havamál"
        [ test "no compression" <|
            \_ ->
                Inflate.inflate (encode Havamal.noCompression)
                    |> Result.map decode
                    |> Expect.equal (Ok (Just Havamal.uncompressed))
        , test "fixed compression" <|
            \_ ->
                Inflate.inflate (encode Havamal.fixed)
                    |> Result.map decode
                    |> Expect.equal (Ok (Just Havamal.uncompressed))
        , test "dynamic compression" <|
            \_ ->
                Inflate.inflate (encode Havamal.compressed)
                    |> Result.map decode
                    |> Expect.equal (Ok (Just Havamal.uncompressed))
        ]


lorem =
    let
        encode v =
            v |> List.map Encode.unsignedInt8 |> Encode.sequence |> Encode.encode

        decode b =
            Decode.decode (ByteArray.decoder (Bytes.width b)) b
                |> Maybe.map Array.toList
    in
    describe "lorem"
        [ test "fixed compression" <|
            \_ ->
                Inflate.inflate (encode Lorem.fixed)
                    |> Result.map decode
                    |> Expect.equal (Ok (Just Lorem.uncompressed))
        , test "no compression" <|
            \_ ->
                Inflate.inflate (encode Lorem.noCompression)
                    |> Result.map decode
                    |> Expect.equal (Ok (Just Lorem.uncompressed))
        , test "dynamic compression" <|
            \_ ->
                Inflate.inflate (encode Lorem.compressed)
                    |> Result.map decode
                    |> Expect.equal (Ok (Just Lorem.uncompressed))
        ]


buildTable =
    let
        lengths =
            Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]

        num =
            1

        offset =
            257

        update : Int -> (a -> a) -> Array a -> Array a
        update index f array =
            case Array.get index array of
                Nothing ->
                    array

                Just v ->
                    Array.set index (f v) array

        withArray _ =
            Array.slice offset (num + offset) lengths
                |> Array.foldl
                    (\i arr ->
                        if i < 16 && i /= 0 then
                            update i (\v -> v + 1) arr

                        else
                            arr
                    )
                    (Array.repeat 16 0)
                |> Array.set 0 0
                |> Array.toList

        withDict _ =
            Array.slice offset (num + offset) lengths
                |> Array.foldl
                    (\i dict ->
                        if i < 16 && i /= 0 then
                            Dict.update i
                                (\maybeValue ->
                                    case maybeValue of
                                        Just v ->
                                            Just (v + 1)

                                        Nothing ->
                                            Just 1
                                )
                                dict

                        else
                            dict
                    )
                    Dict.empty
                |> (\dict -> Dict.foldr folder ( 15, [] ) dict)
                |> (\( current, list ) ->
                        let
                            ( _, result ) =
                                go 0 0 current list
                        in
                        result
                   )

        go : Int -> Int -> Int -> List Int -> ( Int, List Int )
        go key value n accum =
            if key < n then
                go key value (n - 1) (0 :: accum)

            else
                ( key, value :: accum )

        folder key value ( current, list ) =
            go key value current list
    in
    test "build table with dict" <|
        \_ ->
            withDict ()
                |> Expect.equal (withArray ())
