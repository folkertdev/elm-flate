module Inflate.ZLib exposing (inflate)

-- original https://bitbucket.org/jibsen/tinf/src/default/src/tinfzlib.c

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Checksum.Adler32 as Adler32
import Inflate.Internal as Internal


type alias Slices =
    { cmf : Int
    , flg : Int
    , buffer : Bytes
    , a32 : Int
    }


slice : Bytes -> Maybe Slices
slice buffer =
    let
        decoder =
            Decode.map4 Slices
                Decode.unsignedInt8
                Decode.unsignedInt8
                (Decode.bytes (Bytes.width buffer - 2 - 4))
                decodeAdler32Checksum
    in
    Decode.decode decoder buffer


type Error
    = InvalidChecksum
    | InvalidAdler { found : Int, shouldBe : Int }
    | InvalidMethod
    | InvalidWindowSize
    | PresetDictionaryIssue
    | InvalidSlice
    | InflateError String


inflate : Bytes -> Result Error Bytes
inflate buffer =
    case slice buffer of
        Nothing ->
            Err InvalidSlice

        Just sliced ->
            if ((256 * sliced.cmf + sliced.flg) |> modBy 31) /= 0 then
                -- /* check checksum */
                Err InvalidChecksum

            else if Bitwise.and sliced.cmf 0x0F /= 8 then
                -- /* check method is deflate */
                Err InvalidMethod

            else if Bitwise.shiftRightBy 4 sliced.cmf > 7 then
                -- /* check window size is valid */
                Err InvalidWindowSize

            else if Bitwise.and sliced.flg 0x20 /= 0 then
                -- /* check there is no preset dictionary */
                Err PresetDictionaryIssue

            else
                case Internal.inflate sliced.buffer of
                    Err e ->
                        Err (InflateError e)

                    Ok resultBuffer ->
                        let
                            found =
                                Adler32.adler32 resultBuffer
                        in
                        if sliced.a32 /= found then
                            Err (InvalidAdler { found = found, shouldBe = sliced.a32 })

                        else
                            Ok resultBuffer


decodeAdler32Checksum : Decoder Int
decodeAdler32Checksum =
    Decode.unsignedInt32 BE
