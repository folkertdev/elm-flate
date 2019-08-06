module Flate exposing
    ( deflate, inflate
    , deflateGZip, inflateGZip
    , deflateZlib, inflateZlib
    , deflateWithOptions, deflateGZipWithOptions, deflateZlibWithOptions
    , Encoding(..), Compression(..)
    , adler32, crc32
    )

{-| An implementation of [DEFLATE](https://www.ietf.org/rfc/rfc1951.txt) compression.

The deflate format is used in common file formats like gzip, png, and woff.

    import Bytes exposing (Bytes)
    import Bytes.Encode as Encode
    import Bytes.Decode as Decode

    text : String
    text =
        "Dyn flam dôve nea"

    decodeAsString : Bytes -> Maybe String
    decodeAsString buffer =
        let
            decoder = Decode.string (Bytes.width buffer)
        in
            Decode.decode decoder buffer

    inflate (deflate (Encode.encode (Encode.string text)))
        |> Maybe.andThen decodeAsString
        --> Just "Dyn flam dôve nea"

@docs deflate, inflate


## GZip

@docs deflateGZip, inflateGZip


## Zlib

@docs deflateZlib, inflateZlib


## Lowlevel Primitives

@docs deflateWithOptions, deflateGZipWithOptions, deflateZlibWithOptions
@docs Encoding, Compression


## Checksums

Checksums are simple hashes that are used to make check that the decoded data is the same as the encoded data.
The gzip and zlib formats calculate the checksum over the data they encode, and put the value into the output.
When decoding, the checksum is also calculated for the decoded data. The two values must be the same.

@docs adler32, crc32

-}

import Array exposing (Array)
import Bitwise
import ByteArray
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode
import Checksum.Adler32
import Checksum.Crc32
import Deflate.Internal as Internal
import Inflate.Inflate as Inflate
import LZ77


{-| Inflate (decode) data encoded with DEFLATE
-}
inflate : Bytes -> Maybe Bytes
inflate =
    Inflate.inflate


{-| Deflate a sequence of bytes. This is an alias for:

    deflateWithOptions
        (Dynamic (WithWindowSize LZ77.maxWindowSize))

That means good compression, but can be slow for large data.
The README elaborates on deflate performance. `deflateWithOptions` allows you to pick different deflate options.

-}
deflate : Bytes -> Bytes
deflate =
    deflateWithOptions (Dynamic (WithWindowSize LZ77.maxWindowSize))


{-| Deflate a sequence of bytes

    import LZ77
    import Bytes

    data : Bytes.Bytes
    data =
        Encode.encode (Encode.string "foo")

    deflateWithOptions Raw data
        --> [1,3,0,252,-1,102,111,111]


    deflateWithOptions (Static NoCompression) data
        --> [75,203,207,7,0]


    deflateWithOptions (Dynamic (WithWindowSize LZ77.maxWindowSize)) data
        --> [5,192,33,1,0,0,0,128,-96,183,86,254,55,137,1]

-}
deflateWithOptions : Encoding -> Bytes -> Bytes
deflateWithOptions encoding buffer =
    case encoding of
        Raw ->
            Internal.encodeRaw buffer

        Static NoCompression ->
            Internal.encodeStatic Nothing buffer

        Static (WithWindowSize w) ->
            Internal.encodeStatic (Just w) buffer

        Dynamic NoCompression ->
            Internal.encodeDynamic Nothing buffer

        Dynamic (WithWindowSize w) ->
            Internal.encodeDynamic (Just w) buffer


{-| Inflate data compressed with gzip.
gzip adds some extra data at the front and the back. This decoder will take care of that and also check the checksum if specified.

**Note**: this only gives back the inflated data block. The header and trailer parts are discarded, but checksums (if specified) will be checked.

-}
inflateGZip : Bytes -> Maybe Bytes
inflateGZip =
    Inflate.inflateGZip


{-| Deflate with the [gzip](https://tools.ietf.org/html/rfc1952) format

**Note**: the gzip header and trailer are not customizable in the current version.

-}
deflateGZip : Bytes -> Bytes
deflateGZip =
    deflateGZipWithOptions (Dynamic (WithWindowSize LZ77.maxWindowSize))


{-| -}
deflateGZipWithOptions : Encoding -> Bytes -> Bytes
deflateGZipWithOptions encoding buffer =
    let
        data =
            deflateWithOptions encoding buffer

        encodedHeader =
            [ -- id bytes
              Encode.unsignedInt8 31
            , Encode.unsignedInt8 139

            -- method
            , Encode.unsignedInt8 8

            -- flags
            , Encode.unsignedInt8 0

            -- modification time
            , Encode.unsignedInt32 LE 0

            -- compression level: unknown=0
            , Encode.unsignedInt8 0

            -- OS: unknown=255
            , Encode.unsignedInt8 255
            ]

        encodedTrailer =
            [ Encode.unsignedInt32 LE (Checksum.Crc32.crc32 buffer)
            , Encode.unsignedInt32 LE (Bytes.width buffer |> modBy 4294967296)
            ]
    in
    Encode.sequence (encodedHeader ++ [ Encode.bytes data ] ++ encodedTrailer)
        |> Encode.encode


{-| Inflate data compressed with [zlib](http://www.zlib.net/).
zlib adds some extra data at the front and the back. This decoder will take care of that and also check the checksum if specified.

**Note**: this only gives back the inflated data block. The header and trailer parts are discarded, but checksums (if specified) are checked.

-}
inflateZlib : Bytes -> Maybe Bytes
inflateZlib =
    Inflate.inflateZLib


{-| Deflate with the [zlib](https://www.ietf.org/rfc/rfc1950.txt) format

**Note**: the zlib header and trailer are not customizable in the current version.

-}
deflateZlib : Bytes -> Bytes
deflateZlib =
    deflateZlibWithOptions (Dynamic (WithWindowSize LZ77.maxWindowSize))


{-| -}
deflateZlibWithOptions : Encoding -> Bytes -> Bytes
deflateZlibWithOptions encoding buffer =
    let
        data =
            deflateWithOptions encoding buffer

        windowSize =
            case encoding of
                Raw ->
                    0

                Static NoCompression ->
                    0

                Static (WithWindowSize size) ->
                    windowSizeHelp size

                Dynamic NoCompression ->
                    0

                Dynamic (WithWindowSize size) ->
                    windowSizeHelp size

        windowSizeHelp size =
            let
                kb =
                    1024
            in
            if size <= 256 then
                0

            else if size <= 512 then
                1

            else if size <= 1 * kb then
                2

            else if size <= 2 * kb then
                3

            else if size <= 4 * kb then
                4

            else if size <= 8 * kb then
                5

            else if size <= 16 * kb then
                6

            else if size <= 32 * kb then
                7

            else
                -- more than  the max window size of LZ77
                0

        -- 8 indicates the compression method is deflate
        cmf =
            Bitwise.or (Bitwise.shiftLeftBy 4 windowSize) 8

        mask16 value =
            Bitwise.and value ((2 ^ 16) - 1)

        compressionLevel =
            2

        check =
            Bitwise.shiftLeftBy 8 cmf + Bitwise.shiftLeftBy 6 compressionLevel

        flag =
            if (check |> modBy 31) /= 0 then
                Bitwise.shiftLeftBy 6 compressionLevel + (31 - (check |> modBy 31))

            else
                Bitwise.shiftLeftBy 6 compressionLevel

        encodedHeader =
            [ Encode.unsignedInt8 cmf
            , Encode.unsignedInt8 flag
            ]

        encodedTrailer =
            [ Encode.unsignedInt32 BE (Checksum.Adler32.adler32 buffer)
            ]
    in
    Encode.sequence (encodedHeader ++ [ Encode.bytes data ] ++ encodedTrailer)
        |> Encode.encode


{-| The type of encoding used

  - **Raw**: puts the raw bytes into the output. The output is a bit larger, but it is now a valid zip/gzip/zlib compressed byte stream.
  - **Static**: uses a default huffman table to shorten the input. Good for very short messages, or messages with a lot of simple repetition, because the table is not included.
  - **Dynamic**: calculates an optimal huffman table for the specific input it is given. This table must be included in the output, but the result can be much smaller than with the static table.

**default**: `Dynamic`

-}
type Encoding
    = Raw
    | Dynamic Compression
    | Static Compression


{-| The type of compression used

  - **NoCompression**: the data will not be compressed. The encoder still can give a smaller file. Use this option if superquick encoding and/or decoding is needed, and output size is less of a concern.
  - **WithWindowSize**: use [LZ77 compression](https://en.wikipedia.org/wiki/LZ77_and_LZ78#LZ77) with the given window size.

**default**: `WithWindowSize LZ77.maxWindowSize`

-}
type Compression
    = NoCompression
    | WithWindowSize Int



-- Check sums


{-| The adler32 checksum.
Used in zlib. Faster than crc32, but also less reliable (larger chance of collisions).
-}
adler32 : Bytes -> Int
adler32 =
    Checksum.Adler32.adler32


{-| The crc32 checksum.
Used in gzip. Slower than adler32, but also more reliable (smaller chance of collisions).
-}
crc32 : Bytes -> Int
crc32 =
    Checksum.Crc32.crc32
