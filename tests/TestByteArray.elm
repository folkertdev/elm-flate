module TestByteArray exposing (suite)

import Array exposing (Array)
import ByteArray as ArrayHelp
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Experimental.ByteArray as ByteArray
import Fuzz
import Test exposing (..)


suite =
    if True then
        fullSuite

    else
        mySuite


mySuite =
    let
        v =
            List.range 0 19
    in
    copyBackTest 5 6 v


copyBackTest a b v =
    let
        array =
            Array.fromList v

        byteArray =
            ByteArray.fromList v
    in
    test ("copyToBack works like array copy " ++ String.fromInt a ++ " " ++ String.fromInt b) <|
        \_ ->
            let
                forArray =
                    copyLoop a b a (Array.length array) array

                forBArray =
                    ByteArray.copyToBack a b byteArray
            in
            forBArray
                |> ByteArray.toList
                |> Expect.equal (Array.toList forArray)


fullSuite =
    describe "ByteArray"
        [ describe "length"
            [ test "length empty" <|
                \_ ->
                    ByteArray.length ByteArray.empty
                        |> Expect.equal 0
            , test "length singleton" <|
                \_ ->
                    ByteArray.empty
                        |> ByteArray.push 42
                        |> ByteArray.length
                        |> Expect.equal 1
            , test "length 2 elements" <|
                \_ ->
                    ByteArray.empty
                        |> ByteArray.push 42
                        |> ByteArray.push 42
                        |> ByteArray.length
                        |> Expect.equal 2
            , test "length 10 elements" <|
                \_ ->
                    List.foldl ByteArray.push ByteArray.empty (List.range 0 9)
                        |> ByteArray.length
                        |> Expect.equal 10
            ]
        , let
            data =
                List.foldl ByteArray.push ByteArray.empty (List.range 0 9)
          in
          describe "identities"
            [ test "set 3 v >> get 3 = Just v" <|
                \_ ->
                    data
                        |> ByteArray.set 3 42
                        |> ByteArray.get 3
                        |> Expect.equal (Just 42)
            , test "set 4 v >> get 4 = Just v" <|
                \_ ->
                    data
                        |> ByteArray.set 4 42
                        |> ByteArray.get 4
                        |> Expect.equal (Just 42)
            , test "set 0 v >> get 0 = Just v" <|
                \_ ->
                    data
                        |> ByteArray.set 0 42
                        |> ByteArray.get 0
                        |> Expect.equal (Just 42)
            , test "0th number" <|
                \_ ->
                    data
                        |> ByteArray.get 0
                        |> Expect.equal (Just 0)
            , test "1st number" <|
                \_ ->
                    data
                        |> ByteArray.get 1
                        |> Expect.equal (Just 1)
            , test "4th number" <|
                \_ ->
                    data
                        |> ByteArray.get 4
                        |> Expect.equal (Just 4)
            , test "0th number on empty array " <|
                \_ ->
                    ByteArray.empty
                        |> ByteArray.push 42
                        |> ByteArray.push 5
                        |> ByteArray.get 0
                        |> Expect.equal (Just 42)
            , test "range" <|
                \_ ->
                    ByteArray.fromList (List.range 0 10)
                        |> ByteArray.toList
                        |> Expect.equal (List.range 0 10)
            , let
                v =
                    List.range 0 19

                array =
                    Array.fromList v

                byteArray =
                    ByteArray.fromList v
              in
              describe "copyToBack"
                [ test "copyToBack works like array copy 4 8 " <|
                    \_ ->
                        let
                            forArray =
                                copyLoop 4 8 4 (Array.length array) array

                            forBArray =
                                ByteArray.copyToBack 4 8 byteArray
                        in
                        forBArray
                            |> ByteArray.toList
                            |> Expect.equal (Array.toList forArray)
                , test "copyToBack works like array copy 2 8" <|
                    \_ ->
                        let
                            forArray =
                                copyLoop 2 8 2 (Array.length array) array

                            forBArray =
                                ByteArray.copyToBack 2 8 byteArray
                        in
                        forBArray
                            |> ByteArray.toList
                            |> Expect.equal (Array.toList forArray)
                , test "copyToBack works like array copy 1 5" <|
                    \_ ->
                        let
                            forArray =
                                copyLoop 1 5 1 (Array.length array) array

                            forBArray =
                                ByteArray.copyToBack 1 5 byteArray
                        in
                        forBArray
                            |> ByteArray.toList
                            |> Expect.equal (Array.toList forArray)
                , test "copyToBack works like array copy 3 2" <|
                    \_ ->
                        let
                            forArray =
                                copyLoop 3 2 3 (Array.length array) array

                            forBArray =
                                ByteArray.copyToBack 3 2 byteArray
                        in
                        forBArray
                            |> ByteArray.toList
                            |> Expect.equal (Array.toList forArray)
                , test "copyToBack works like array copy 0 1" <|
                    \_ ->
                        let
                            forArray =
                                copyLoop 0 1 0 (Array.length array) array

                            forBArray =
                                ByteArray.copyToBack 0 1 byteArray
                        in
                        forBArray
                            |> ByteArray.toList
                            |> Expect.equal (Array.toList forArray)
                , fuzz (Fuzz.tuple ( Fuzz.intRange 0 19, Fuzz.intRange 0 19 )) "copyToBack fuzz" <|
                    \( startIndex, length ) ->
                        let
                            forArray =
                                copyLoop startIndex length startIndex (Array.length array) array

                            forBArray =
                                ByteArray.copyToBack startIndex length byteArray
                        in
                        forBArray
                            |> ByteArray.toList
                            |> Expect.equal (Array.toList forArray)
                ]
            ]
        , describe "list conversion"
            [ fuzz (Fuzz.list (Fuzz.intRange 0 255)) "fromBytes" <|
                \bytes ->
                    bytes
                        |> ByteArray.fromList
                        |> ByteArray.toList
                        |> Expect.equal bytes
            ]
        , describe "byte conversion"
            [ test "fromBytes" <|
                \_ ->
                    let
                        range =
                            List.range 0 102

                        encoded =
                            range
                                |> List.map Encode.unsignedInt8
                                |> Encode.sequence
                                |> Encode.encode
                    in
                    encoded
                        |> ByteArray.fromBytes
                        |> ByteArray.toList
                        |> Expect.equal range
            , test "fromBytes [0,0,0,0]" <|
                \_ ->
                    let
                        range =
                            [ 0, 0, 0, 0 ]

                        encoded =
                            range
                                |> List.map Encode.unsignedInt8
                                |> Encode.sequence
                                |> Encode.encode
                    in
                    encoded
                        |> ByteArray.fromBytes
                        |> ByteArray.toList
                        |> Expect.equal range
            , fuzz (Fuzz.list (Fuzz.intRange 0 255)) "fromBytes fuzz" <|
                \range ->
                    let
                        encoded =
                            range
                                |> List.map Encode.unsignedInt8
                                |> Encode.sequence
                                |> Encode.encode
                    in
                    encoded
                        |> ByteArray.fromBytes
                        |> ByteArray.toList
                        |> Expect.equal range
            , test "toBytes" <|
                \_ ->
                    let
                        array =
                            List.range 0 102
                                |> Array.fromList

                        byteArray =
                            List.range 0 102
                                |> ByteArray.fromList
                    in
                    byteArray
                        |> ByteArray.toBytes
                        |> ArrayHelp.fromBytes
                        |> Expect.equal (ArrayHelp.toBytes array |> ArrayHelp.fromBytes)
            , fuzz (Fuzz.list (Fuzz.intRange 0 255)) "toBytes fuzz" <|
                \range ->
                    let
                        array =
                            range
                                |> Array.fromList

                        byteArray =
                            range
                                |> ByteArray.fromList
                    in
                    byteArray
                        |> ByteArray.toBytes
                        |> ArrayHelp.fromBytes
                        |> Expect.equal (ArrayHelp.toBytes array |> ArrayHelp.fromBytes)
            ]
        , appendBytesTests
        ]


appendBytesTests =
    let
        bytes =
            [ 1, 2, 3, 4, 5 ]
                |> List.map Encode.unsignedInt8
                |> Encode.sequence
                |> Encode.encode
    in
    describe "appendBytes"
        [ test "add bytes to []" <|
            \_ ->
                ByteArray.empty
                    |> ByteArray.appendBytes bytes
                    |> ByteArray.toBytes
                    |> ArrayHelp.fromBytes
                    |> Expect.equal (Array.fromList [ 1, 2, 3, 4, 5 ])
        , test "add bytes to [11]" <|
            \_ ->
                ByteArray.empty
                    |> ByteArray.push 11
                    |> ByteArray.appendBytes bytes
                    |> ByteArray.toBytes
                    |> ArrayHelp.fromBytes
                    |> Expect.equal (Array.fromList [ 11, 1, 2, 3, 4, 5 ])
        , test "add bytes to [11, 12]" <|
            \_ ->
                ByteArray.empty
                    |> ByteArray.push 11
                    |> ByteArray.push 12
                    |> ByteArray.appendBytes bytes
                    |> ByteArray.toBytes
                    |> ArrayHelp.fromBytes
                    |> Expect.equal (Array.fromList [ 11, 12, 1, 2, 3, 4, 5 ])
        ]


copyLoop : Int -> Int -> Int -> Int -> Array a -> Array a
copyLoop offs length i destLen arr =
    if (i - (offs + length)) < 0 then
        copyLoop offs
            length
            (i + 1)
            (destLen + 1)
            (let
                source =
                    i

                destination =
                    destLen
             in
             case Array.get source arr of
                Nothing ->
                    arr

                Just value ->
                    let
                        size =
                            Array.length arr
                    in
                    if (destination - size) < 0 then
                        Array.set destination value arr

                    else if (destination - size) == 0 then
                        Array.push value arr

                    else
                        arr
            )

    else
        arr
