module Inflate.GZip exposing (inflate)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Checksum.Crc32 as Crc32
import Inflate.Internal as Internal


inflate : Bytes -> Maybe Bytes
inflate buffer =
    gzipSlice buffer
        |> Maybe.andThen gzipFindBuffer
        |> Maybe.andThen (Internal.inflate >> Result.toMaybe)


flags =
    { text = 1
    , crc = 2
    , extra = 4
    , name = 8
    , comment = 16
    }


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap argument function =
    Decode.map2 (<|) function argument


type alias GzipSlice =
    { orig : Bytes
    , id1 : Int
    , id2 : Int
    , method : Int
    , flg : Int
    , restOfHeader : Bytes
    , buffer : Bytes
    , crc32 : Int
    , decompressedLength : Int
    }


gzipSlice : Bytes -> Maybe GzipSlice
gzipSlice buffer =
    let
        decoder =
            Decode.succeed (GzipSlice buffer)
                |> andMap Decode.unsignedInt8
                |> andMap Decode.unsignedInt8
                |> andMap Decode.unsignedInt8
                |> andMap Decode.unsignedInt8
                |> andMap (Decode.bytes 6)
                |> andMap (Decode.bytes (Bytes.width buffer - 10 - 4 - 4))
                |> andMap (Decode.unsignedInt32 LE)
                |> andMap (Decode.unsignedInt32 LE)
    in
    Decode.decode decoder buffer


gzipFindBuffer : GzipSlice -> Maybe Bytes
gzipFindBuffer sliced =
    -- check id bytes
    if sliced.id1 /= 0x1F || sliced.id2 /= 0x8B then
        Nothing
        -- /* check method is deflate */

    else if sliced.method /= 8 then
        Nothing
        -- /* check that reserved bits are zero */

    else if Bitwise.and sliced.flg 0xE0 /= 0 then
        Nothing

    else
        let
            flagSet : Int -> Bool
            flagSet flag =
                Bitwise.and sliced.flg flag /= 0

            skipExtra =
                if flagSet flags.extra then
                    Decode.unsignedInt16 LE
                        |> Decode.andThen (\extraSize -> Decode.bytes extraSize |> Decode.map (\_ -> extraSize + 2))

                else
                    Decode.succeed 0

            skipFileName =
                if flagSet flags.name then
                    skipUntilZero

                else
                    Decode.succeed 0

            skipFileComment =
                if flagSet flags.comment then
                    skipUntilZero

                else
                    Decode.succeed 0

            checkHeaderCrc bytesRead =
                if flagSet flags.crc then
                    Decode.unsignedInt16 LE
                        |> Decode.andThen
                            (\checksum ->
                                case Decode.decode (Decode.bytes bytesRead) sliced.orig of
                                    Just header ->
                                        if checksum /= Bitwise.and (Crc32.crc32 header) 0xFFFF then
                                            Decode.fail

                                        else
                                            Decode.succeed 2

                                    Nothing ->
                                        Decode.fail
                            )

                else
                    Decode.succeed 0

            skipAll =
                Decode.succeed (\a b c -> a + b + c)
                    |> andMap skipExtra
                    |> andMap skipFileName
                    |> andMap skipFileComment

            headerSize =
                10

            decoder =
                skipAll
                    |> Decode.andThen
                        (\skipped0 ->
                            checkHeaderCrc (skipped0 + headerSize)
                                |> Decode.andThen
                                    (\skipped1 ->
                                        let
                                            skipped =
                                                skipped0 + skipped1
                                        in
                                        Decode.bytes (Bytes.width sliced.buffer - skipped)
                                    )
                        )
        in
        Decode.decode decoder sliced.buffer


skipUntilZero : Decoder Int
skipUntilZero =
    let
        go n =
            Decode.unsignedInt8
                |> Decode.map
                    (\byte ->
                        if byte == 0 then
                            Decode.Done (n + 1)

                        else
                            Decode.Loop (n + 1)
                    )
    in
    Decode.loop 0 go
