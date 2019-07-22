module ByteArray exposing (decoder, fel, felList, fer, fromBytes)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)


fromBytes : Bytes -> Array Int
fromBytes buffer =
    case Decode.decode (decodeByteArray (Bytes.width buffer)) buffer of
        Nothing ->
            Array.empty

        Just value ->
            value


decoder : Int -> Decoder (Array Int)
decoder =
    decodeByteArray


decodeByteArray : Int -> Decoder (Array Int)
decodeByteArray n =
    Decode.loop ( n, Array.empty ) decodeByteArrayHelp


{-| Decode a byte array, but push the elements onto an existing array
-}
decodeByteArrayLowLevel : Int -> Array Int -> Decoder (Array Int)
decodeByteArrayLowLevel n initial =
    Decode.loop ( n, initial ) decodeByteArrayHelp


decodeByteArrayHelp : ( Int, Array Int ) -> Decoder (Decode.Step ( Int, Array Int ) (Array Int))
decodeByteArrayHelp ( remaining, accum ) =
    if remaining >= 4 then
        Decode.unsignedInt32 BE
            |> Decode.map
                (\new ->
                    let
                        byte1 =
                            Bitwise.shiftRightBy 24 new
                                |> Bitwise.shiftRightZfBy 0
                                |> Bitwise.and 0xFF

                        byte2 =
                            Bitwise.shiftRightBy 16 new
                                |> Bitwise.shiftRightZfBy 0
                                |> Bitwise.and 0xFF

                        byte3 =
                            Bitwise.shiftRightBy 8 new
                                |> Bitwise.shiftRightZfBy 0
                                |> Bitwise.and 0xFF

                        byte4 =
                            new
                                |> Bitwise.shiftRightZfBy 0
                                |> Bitwise.and 0xFF

                        newAccum =
                            accum
                                |> Array.push byte1
                                |> Array.push byte2
                                |> Array.push byte3
                                |> Array.push byte4
                    in
                    Decode.Loop ( remaining - 4, newAccum )
                )

    else if remaining > 0 then
        Decode.unsignedInt8 |> Decode.map (\new -> Decode.Loop ( remaining - 1, Array.push new accum ))

    else
        Decode.succeed (Decode.Done accum)


fer : Array Int -> Bytes
fer array =
    Array.foldr fasterEncodeFolderR ( 0, 0, [] ) array
        |> fasterEncodeR
        |> Encode.sequence
        |> Encode.encode


fel : Array Int -> Bytes
fel array =
    Array.foldl fasterEncodeFolderL ( 0, 0, [] ) array
        |> fasterEncodeL
        |> Encode.sequence
        |> Encode.encode


felList : List Int -> Bytes
felList array =
    Array.fromList array
        |> fer


fasterEncodeR ( bytesOnAccum, accum, otherEncoders ) =
    let
        encoders =
            case bytesOnAccum of
                0 ->
                    otherEncoders

                1 ->
                    Encode.unsignedInt8 accum :: otherEncoders

                2 ->
                    Encode.unsignedInt16 BE accum :: otherEncoders

                _ ->
                    let
                        firstByte =
                            Bitwise.and 0xFF accum

                        otherBytes =
                            Bitwise.shiftRightBy 8 accum
                    in
                    Encode.unsignedInt16 BE otherBytes :: Encode.unsignedInt8 firstByte :: otherEncoders
    in
    encoders


fasterEncodeFolderR byte ( bytesOnAccum, accum, encoders ) =
    case bytesOnAccum of
        0 ->
            let
                value =
                    byte
                        |> Bitwise.and 0xFF
            in
            ( 1, value, encoders )

        1 ->
            let
                value =
                    byte
                        |> Bitwise.and 0xFF
                        |> Bitwise.shiftLeftBy 8
                        |> Bitwise.or accum
            in
            ( 2, value, encoders )

        2 ->
            let
                value =
                    byte
                        |> Bitwise.and 0xFF
                        |> Bitwise.shiftLeftBy 16
                        |> Bitwise.or accum
            in
            ( 3, value, encoders )

        _ ->
            let
                value =
                    byte
                        |> Bitwise.and 0xFF
                        |> Bitwise.shiftLeftBy 24
                        |> Bitwise.or accum
            in
            ( 0, 0, Encode.unsignedInt32 BE value :: encoders )


fasterEncodeL ( bytesOnAccum, accum, otherEncoders ) =
    let
        encoders =
            case bytesOnAccum of
                0 ->
                    otherEncoders

                1 ->
                    Encode.unsignedInt8 accum :: otherEncoders

                2 ->
                    Encode.unsignedInt16 BE accum :: otherEncoders

                _ ->
                    let
                        firstByte =
                            Bitwise.and 0xFF accum

                        otherBytes =
                            Bitwise.shiftRightBy 8 accum
                    in
                    Encode.unsignedInt8 firstByte :: Encode.unsignedInt16 BE otherBytes :: otherEncoders
    in
    encoders


fasterEncodeFolderL byte ( bytesOnAccum, accum, encoders ) =
    case bytesOnAccum of
        0 ->
            ( 1, Bitwise.and 0xFF byte, encoders )

        1 ->
            let
                value =
                    Bitwise.or (Bitwise.shiftLeftBy 8 accum) (Bitwise.and 0xFF byte)
            in
            ( 2, value, encoders )

        2 ->
            let
                value =
                    Bitwise.or (Bitwise.shiftLeftBy 8 accum) (Bitwise.and 0xFF byte)
            in
            ( 3, value, encoders )

        _ ->
            let
                value =
                    Bitwise.or (Bitwise.shiftLeftBy 8 accum) (Bitwise.and 0xFF byte)
            in
            ( 0, 0, Encode.unsignedInt32 BE value :: encoders )
