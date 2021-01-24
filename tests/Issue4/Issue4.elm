module Issue4.Issue4 exposing (suite)

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Flate
import Inflate.Internal as Inflate
import LZ77
import Test exposing (..)



-- See https://github.com/folkertdev/elm-flate/issues/4 for a full description of the problem


suite =
    describe "Issue4"
        [ test "all" <|
            \_ ->
                data
                    |> Inflate.inflate
                    |> Expect.equal (Ok (Encode.encode (Encode.sequence [])))
        , test "tail" <|
            \_ ->
                data2
                    |> Inflate.inflate
                    |> Expect.equal (Ok (Encode.encode (Encode.sequence [])))
        ]


data2 =
    [ 0, 0, 0, 255, 255, 3, 0 ]
        |> List.map Encode.unsignedInt8
        |> Encode.sequence
        |> Encode.encode


data =
    raw
        |> List.map Encode.unsignedInt8
        |> Encode.sequence
        |> Encode.encode


raw : List Int
raw =
    [ 100, 143, 65, 107, 2, 49, 16, 133, 239, 66, 255, 67, 152, 123, 205, 218, 131, 20, 73, 226, 161, 80, 208, 246, 32, 84, 127, 192, 176, 59, 186, 129, 100, 178, 102, 102, 75, 253, 247, 221, 82, 68, 208, 227, 247, 189, 247, 14, 207, 173, 127, 114, 50, 223, 84, 37, 22, 246, 176, 152, 55, 96, 136, 219, 210, 69, 62, 121, 56, 236, 223, 159, 95, 193, 136, 34, 119, 152, 10, 147, 135, 11, 9, 172, 195, 211, 204, 137, 168, 153, 182, 44, 30, 122, 213, 97, 101, 173, 180, 61, 101, 148, 121, 25, 136, 167, 228, 88, 106, 70, 157, 176, 158, 172, 12, 149, 176, 147, 158, 72, 115, 178, 47, 77, 179, 180, 25, 35, 131, 105, 203, 200, 234, 97, 9, 102, 228, 120, 30, 233, 237, 202, 193, 73, 12, 78, 195, 246, 195, 89, 13, 206, 254, 209, 191, 217, 61, 152, 13, 71, 141, 152, 228, 190, 249, 133, 9, 235, 229, 222, 126, 238, 30, 122, 138, 85, 111, 210, 78, 215, 194, 47, 0, 0, 0, 255, 255, 3, 0 ]
