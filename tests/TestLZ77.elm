module TestLZ77 exposing (suite)

import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import LZ77 exposing (Code(..))
import Test exposing (..)


suite : Test
suite =
    describe "LZ77"
        [ test "aaaaa" <|
            \_ ->
                let
                    bytes =
                        Encode.encode (Encode.string "aaaaa")

                    expected =
                        [ Literal 97, Pointer 4 1 ]
                in
                LZ77.encode bytes
                    |> Expect.equal expected
        , test "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" <|
            \_ ->
                let
                    bytes =
                        Encode.encode (Encode.string "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

                    expected =
                        [ Literal 97, Pointer 58 1 ]
                in
                LZ77.encodeWithOptions { windowSize = 20 } bytes
                    |> Expect.equal expected
        , test "random filler words" <|
            \_ ->
                let
                    text =
                        "foo bar bar foo foo spam random filler words foo foo"

                    expected =
                        [ Literal 102, Literal 111, Literal 111, Literal 32, Literal 98, Literal 97, Literal 114, share { length = 5, distance = 4 }, Literal 102, Literal 111, Literal 111, share { length = 5, distance = 4 }, Literal 115, Literal 112, Literal 97, Literal 109, Literal 32, Literal 114, Literal 97, Literal 110, Literal 100, Literal 111, Literal 109, Literal 32, Literal 102, Literal 105, Literal 108, Literal 108, Literal 101, Literal 114, Literal 32, Literal 119, Literal 111, Literal 114, Literal 100, Literal 115, Literal 32, Literal 102, Literal 111, Literal 111, share { length = 4, distance = 4 } ]
                in
                LZ77.encodeWithOptions { windowSize = 10 } (Encode.encode (Encode.string text))
                    |> Expect.equal expected
        , test "random filler words roundtrip" <|
            \_ ->
                let
                    text =
                        "foo bar bar foo foo spam random filler words foo foo"
                in
                LZ77.encodeWithOptions { windowSize = 10 } (Encode.encode (Encode.string text))
                    |> LZ77.decode
                    |> Decode.decode (Decode.string (Encode.getStringWidth text))
                    |> Expect.equal (Just text)
        , frankenstein11
        ]


share { length, distance } =
    Pointer length distance


frankenstein11 =
    test "frankenstein 1.1" <|
        \_ ->
            let
                text =
                    """I am by birth a Genevese, and my family is one of the most distinguished of that republic. My ancestors had been for many years counsellors and syndics, and my father had filled several public situations with honour and reputation. He was respected by all who knew him for his integrity and indefatigable attention to public business. He passed his younger days perpetually occupied by the affairs of his country; a variety of circumstances had prevented his marrying early, nor was it until the decline of life that he became a husband and the father of a family."""
            in
            LZ77.encodeWithOptions { windowSize = 60 } (Encode.encode (Encode.string text))
                |> Expect.equal frankenstein11Expected


frankenstein11Expected =
    [ Literal 73
    , Literal 32
    , Literal 97
    , Literal 109
    , Literal 32
    , Literal 98
    , Literal 121
    , Literal 32
    , Literal 98
    , Literal 105
    , Literal 114
    , Literal 116
    , Literal 104
    , Literal 32
    , Literal 97
    , Literal 32
    , Literal 71
    , Literal 101
    , Literal 110
    , Literal 101
    , Literal 118
    , Literal 101
    , Literal 115
    , Literal 101
    , Literal 44
    , Literal 32
    , Literal 97
    , Literal 110
    , Literal 100
    , Literal 32
    , Literal 109
    , Literal 121
    , Literal 32
    , Literal 102
    , Literal 97
    , Literal 109
    , Literal 105
    , Literal 108
    , Literal 121
    , Literal 32
    , Literal 105
    , Literal 115
    , Literal 32
    , Literal 111
    , Literal 110
    , Literal 101
    , Literal 32
    , Literal 111
    , Literal 102
    , Literal 32
    , Literal 116
    , Literal 104
    , Literal 101
    , Literal 32
    , Literal 109
    , Literal 111
    , Literal 115
    , Literal 116
    , Literal 32
    , Literal 100
    , Literal 105
    , Literal 115
    , Literal 116
    , Literal 105
    , Literal 110
    , Literal 103
    , Literal 117
    , Literal 105
    , Literal 115
    , Literal 104
    , Literal 101
    , Literal 100
    , share { length = 6, distance = 26 }
    , Literal 97
    , Literal 116
    , Literal 32
    , Literal 114
    , Literal 101
    , Literal 112
    , Literal 117
    , Literal 98
    , Literal 108
    , Literal 105
    , Literal 99
    , Literal 46
    , Literal 32
    , Literal 77
    , Literal 121
    , Literal 32
    , Literal 97
    , Literal 110
    , Literal 99
    , Literal 101
    , Literal 115
    , Literal 116
    , Literal 111
    , Literal 114
    , Literal 115
    , Literal 32
    , Literal 104
    , Literal 97
    , Literal 100
    , Literal 32
    , Literal 98
    , Literal 101
    , Literal 101
    , Literal 110
    , Literal 32
    , Literal 102
    , Literal 111
    , Literal 114
    , Literal 32
    , Literal 109
    , Literal 97
    , Literal 110
    , Literal 121
    , Literal 32
    , Literal 121
    , Literal 101
    , Literal 97
    , share { length = 3, distance = 24 }
    , Literal 99
    , Literal 111
    , Literal 117
    , Literal 110
    , Literal 115
    , Literal 101
    , Literal 108
    , Literal 108
    , share { length = 4, distance = 36 }
    , Literal 97
    , Literal 110
    , Literal 100
    , Literal 32
    , Literal 115
    , Literal 121
    , Literal 110
    , Literal 100
    , Literal 105
    , Literal 99
    , Literal 115
    , Literal 44
    , share { length = 5, distance = 13 }
    , Literal 109
    , Literal 121
    , Literal 32
    , Literal 102
    , Literal 97
    , Literal 116
    , Literal 104
    , Literal 101
    , Literal 114
    , Literal 32
    , Literal 104
    , Literal 97
    , Literal 100
    , Literal 32
    , Literal 102
    , Literal 105
    , Literal 108
    , Literal 108
    , Literal 101
    , share { length = 3, distance = 34 }
    , Literal 101
    , Literal 118
    , Literal 101
    , Literal 114
    , Literal 97
    , Literal 108
    , Literal 32
    , Literal 112
    , Literal 117
    , Literal 98
    , Literal 108
    , Literal 105
    , Literal 99
    , Literal 32
    , Literal 115
    , Literal 105
    , Literal 116
    , Literal 117
    , Literal 97
    , Literal 116
    , Literal 105
    , Literal 111
    , Literal 110
    , Literal 115
    , Literal 32
    , Literal 119
    , Literal 105
    , Literal 116
    , Literal 104
    , Literal 32
    , Literal 104
    , Literal 111
    , Literal 110
    , Literal 111
    , Literal 117
    , Literal 114
    , Literal 32
    , Literal 97
    , Literal 110
    , Literal 100
    , Literal 32
    , Literal 114
    , Literal 101
    , Literal 112
    , Literal 117
    , Literal 116
    , share { length = 5, distance = 28 }
    , Literal 46
    , Literal 32
    , Literal 72
    , Literal 101
    , Literal 32
    , Literal 119
    , Literal 97
    , Literal 115
    , share { length = 3, distance = 19 }
    , Literal 115
    , Literal 112
    , Literal 101
    , Literal 99
    , Literal 116
    , Literal 101
    , Literal 100
    , Literal 32
    , Literal 98
    , Literal 121
    , Literal 32
    , Literal 97
    , Literal 108
    , Literal 108
    , Literal 32
    , Literal 119
    , Literal 104
    , Literal 111
    , Literal 32
    , Literal 107
    , Literal 110
    , Literal 101
    , Literal 119
    , Literal 32
    , Literal 104
    , Literal 105
    , Literal 109
    , Literal 32
    , Literal 102
    , Literal 111
    , Literal 114
    , share { length = 3, distance = 8 }
    , Literal 115
    , Literal 32
    , Literal 105
    , Literal 110
    , Literal 116
    , Literal 101
    , Literal 103
    , Literal 114
    , Literal 105
    , Literal 116
    , share { length = 3, distance = 35 }
    , Literal 110
    , Literal 100
    , share { length = 3, distance = 14 }
    , Literal 100
    , Literal 101
    , Literal 102
    , Literal 97
    , Literal 116
    , Literal 105
    , Literal 103
    , Literal 97
    , Literal 98
    , Literal 108
    , Literal 101
    , Literal 32
    , Literal 97
    , Literal 116
    , Literal 116
    , Literal 101
    , Literal 110
    , Literal 116
    , Literal 105
    , Literal 111
    , Literal 110
    , Literal 32
    , Literal 116
    , Literal 111
    , Literal 32
    , Literal 112
    , Literal 117
    , Literal 98
    , Literal 108
    , Literal 105
    , Literal 99
    , Literal 32
    , Literal 98
    , Literal 117
    , Literal 115
    , Literal 105
    , Literal 110
    , Literal 101
    , Literal 115
    , Literal 115
    , Literal 46
    , Literal 32
    , Literal 72
    , Literal 101
    , Literal 32
    , Literal 112
    , Literal 97
    , Literal 115
    , Literal 115
    , Literal 101
    , Literal 100
    , Literal 32
    , Literal 104
    , Literal 105
    , Literal 115
    , Literal 32
    , Literal 121
    , Literal 111
    , Literal 117
    , Literal 110
    , Literal 103
    , Literal 101
    , Literal 114
    , Literal 32
    , Literal 100
    , Literal 97
    , Literal 121
    , Literal 115
    , Literal 32
    , Literal 112
    , Literal 101
    , Literal 114
    , Literal 112
    , Literal 101
    , Literal 116
    , Literal 117
    , Literal 97
    , Literal 108
    , Literal 108
    , Literal 121
    , Literal 32
    , Literal 111
    , Literal 99
    , Literal 99
    , Literal 117
    , Literal 112
    , Literal 105
    , share { length = 3, distance = 38 }
    , Literal 98
    , Literal 121
    , Literal 32
    , Literal 116
    , Literal 104
    , Literal 101
    , Literal 32
    , Literal 97
    , Literal 102
    , Literal 102
    , Literal 97
    , Literal 105
    , Literal 114
    , Literal 115
    , Literal 32
    , Literal 111
    , Literal 102
    , share { length = 5, distance = 56 }
    , Literal 99
    , share { length = 3, distance = 56 }
    , Literal 116
    , Literal 114
    , Literal 121
    , Literal 59
    , Literal 32
    , Literal 97
    , Literal 32
    , Literal 118
    , Literal 97
    , Literal 114
    , Literal 105
    , Literal 101
    , Literal 116
    , share { length = 3, distance = 50 }
    , Literal 102
    , Literal 32
    , Literal 99
    , Literal 105
    , Literal 114
    , Literal 99
    , Literal 117
    , Literal 109
    , Literal 115
    , Literal 116
    , Literal 97
    , Literal 110
    , Literal 99
    , Literal 101
    , Literal 115
    , Literal 32
    , Literal 104
    , Literal 97
    , Literal 100
    , Literal 32
    , Literal 112
    , Literal 114
    , Literal 101
    , Literal 118
    , Literal 101
    , Literal 110
    , Literal 116
    , Literal 101
    , Literal 100
    , share { length = 5, distance = 54 }
    , Literal 109
    , Literal 97
    , Literal 114
    , Literal 114
    , Literal 121
    , Literal 105
    , Literal 110
    , Literal 103
    , Literal 32
    , Literal 101
    , Literal 97
    , Literal 114
    , Literal 108
    , Literal 121
    , Literal 44
    , Literal 32
    , Literal 110
    , Literal 111
    , Literal 114
    , Literal 32
    , Literal 119
    , Literal 97
    , Literal 115
    , Literal 32
    , Literal 105
    , Literal 116
    , Literal 32
    , Literal 117
    , Literal 110
    , Literal 116
    , Literal 105
    , Literal 108
    , Literal 32
    , Literal 116
    , Literal 104
    , Literal 101
    , Literal 32
    , Literal 100
    , Literal 101
    , Literal 99
    , Literal 108
    , Literal 105
    , Literal 110
    , Literal 101
    , Literal 32
    , Literal 111
    , Literal 102
    , Literal 32
    , Literal 108
    , Literal 105
    , Literal 102
    , Literal 101
    , share { length = 3, distance = 20 }
    , Literal 97
    , Literal 116
    , Literal 32
    , share { length = 3, distance = 24 }
    , Literal 98
    , Literal 101
    , Literal 99
    , Literal 97
    , Literal 109
    , Literal 101
    , Literal 32
    , Literal 97
    , Literal 32
    , Literal 104
    , Literal 117
    , Literal 115
    , Literal 98
    , Literal 97
    , Literal 110
    , Literal 100
    , Literal 32
    , share { length = 4, distance = 4 }
    , share { length = 4, distance = 49 }
    , Literal 102
    , Literal 97
    , share { length = 3, distance = 6 }
    , Literal 114
    , share { length = 4, distance = 48 }
    , Literal 97
    , share { length = 3, distance = 12 }
    , Literal 109
    , Literal 105
    , Literal 108
    , Literal 121
    , Literal 46
    ]
