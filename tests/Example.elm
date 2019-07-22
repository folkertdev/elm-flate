module Example exposing (test)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Flate
import Test


{-| Some sample text
-}
text : String
text =
    "Myn geast ferdizet"


{-| The `text` compressed with raw deflate

This [interactive tool](https://gchq.github.io/CyberChef/#recipe=Raw_Deflate('Fixed%20Huffman%20Coding')To_Hex('Space')&input=QWwgaG9lIHRzanVzdGVyIGl0IGVrIHdpZSwgZHluIGZsYW0gZPR2ZSBuZWE) was used to get this data.

The bytes are combined into 32bit integers using hex notation so they are shorter in the docs

-}
textCompressedBytes =
    [ 0xF3ADCC53, 0x484F4D2C, 0x2E51484B, 0x2D4AC9AC, 0x4A2D0100 ]
        |> List.map (Encode.unsignedInt32 BE)
        |> Encode.sequence
        |> Encode.encode


decodeString : Bytes -> Maybe String
decodeString buffer =
    Decode.decode (Decode.string (Encode.getStringWidth text)) buffer


decompressed =
    textCompressedBytes
        |> Flate.inflate
        |> Maybe.andThen decodeString
        |> Maybe.withDefault ""


test =
    Test.test "example works as shown" <|
        \_ ->
            decompressed
                |> Expect.equal text
