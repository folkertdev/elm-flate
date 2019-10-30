module Issue2 exposing (suite)

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Flate
import Issue2Data
import LZ77
import Test exposing (..)


{-| Solution description

bit values were not correctly masked in the prefix table insert operation. This could cause overflow (e.g. -1 or stuff like that) that would give incorrect matches.

-}
suite =
    describe "issue 2"
        [ test "bug report" <|
            \_ ->
                let
                    size =
                        List.length Issue2Data.expected

                    v =
                        size - 100
                in
                Issue2Data.input
                    |> Flate.inflateGZip
                    |> Maybe.andThen (\b -> Decode.decode (unsignedInt8List (Bytes.width b)) b)
                    |> Maybe.map List.length
                    |> Expect.equal (Just (List.length Issue2Data.expected))
        , test "first bytes" <|
            \_ ->
                case Issue2Data.input |> Flate.inflateGZip |> Maybe.andThen (\b -> Decode.decode (unsignedInt8List (Bytes.width b)) b) of
                    Just v ->
                        let
                            size =
                                List.length v

                            n =
                                900
                        in
                        List.drop (size - n) v
                            |> List.take 100
                            |> Expect.equal (List.take 100 <| List.drop (size - n) Issue2Data.expected)

                    Nothing ->
                        Just []
                            |> Expect.equal (Just (List.take 100 <| List.drop 0 Issue2Data.expected))
        ]


logWidth name v =
    let
        _ =
            Debug.log name (Bytes.width v)
    in
    v


encodeUnsignedInt8 v =
    Encode.encode (Encode.sequence (List.map Encode.unsignedInt8 v))


helpHelp ( n, accum ) =
    if n > 0 then
        Decode.map (\new -> Decode.Loop ( n - 1, new :: accum )) Decode.unsignedInt8

    else
        Decode.succeed (Decode.Done (List.reverse accum))


unsignedInt8List n =
    Decode.loop ( n, [] ) helpHelp
