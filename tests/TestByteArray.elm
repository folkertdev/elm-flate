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
        , describe "byte conversion"
            [ test "fromBytes" <|
                \_ ->
                    let
                        encoded =
                            List.range 0 102
                                |> List.map Encode.unsignedInt8
                                |> Encode.sequence
                                |> Encode.encode
                    in
                    encoded
                        |> ByteArray.fromBytes
                        |> ByteArray.toList
                        |> Expect.equal (ArrayHelp.fromBytes encoded |> Array.toList)
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
            ]
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
