module LZ77 exposing
    ( encode, decode
    , Code(..)
    , encodeWithOptions, maxWindowSize
    )

{-| LZ77 finds sequences of bytes that occur multiple times, and stores them only once:

    LZ77.encode (Encode.encode (Encode.string "aaaaa"))
        --> [ Literal 97, Pointer 4 1 ]

The character `a` occurs 5 times, which is encoded as:

  - the byte 97 (the ascii code for `a`)
  - go back 1 position and read 4 bytes there, putting them onto the end of the stream

Note that the pointer tries to read 4 bytes, even though the output stream at that point only has length 1. This is fine: the elements are copied over one by one.
The general concept behind this kind of compression is [run-length encoding](https://en.wikipedia.org/wiki/Run-length_encoding).

@docs encode, decode
@docs Code
@docs encodeWithOptions, maxWindowSize

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode exposing (Encoder)
import Experimental.ByteArray as ByteArray exposing (ByteArray)
import PrefixTable exposing (PrefixTable)


{-| Encode using the LZ77 encoding
-}
encode : Bytes -> Array Code
encode buffer =
    encodeWithOptions { windowSize = maxWindowSize } buffer


{-| Encode using the LZ77 encoding, with additional options.

  - **window size**: The window size is an how far back a `Pointer` can jump. A bigger window size gives better compression, but requires more data in memory.
    That is almost never a problem nowadays though, so `encode` uses the maximum window size that LZ77 supports.

> **Note**: decreasing the window size doesn't change the performance that much in elm. The bottleneck is in keeping track of matches in a large array, and the size of that array is constant.

-}
encodeWithOptions : { windowSize : Int } -> Bytes -> Array Code
encodeWithOptions { windowSize } buffer =
    flush windowSize (ByteArray.fromBytes buffer)


{-| Decode using the LZ77 encoding
-}
decode : Array Code -> Bytes
decode codes =
    let
        go remaining buffer =
            case remaining of
                [] ->
                    buffer

                (Literal v) :: rest ->
                    go rest (Array.push v buffer)

                (Pointer 0 distance) :: rest ->
                    go rest buffer

                (Pointer length distance) :: rest ->
                    let
                        value =
                            Array.get (size - distance) buffer
                                |> Maybe.withDefault 0

                        size =
                            Array.length buffer
                    in
                    go (Pointer (length - 1) distance :: rest) (Array.push value buffer)
    in
    go (Array.toList codes) Array.empty
        |> Array.toList
        |> List.map Encode.unsignedInt8
        |> Encode.sequence
        |> Encode.encode


{-| The codes

  - **Literal**: A byte value
  - **Pointer length distance**: Go `distance` positions back and read `length` bytes. put the read bytes at the end of the output stream.

-}
type Code
    = Literal Int
    | Pointer Int Int


{-| Maximum length of sharable bytes in a pointer.
-}
max_length =
    258


{-| Maximum backward distance of a pointer.
-}
max_distance =
    32768


{-| Maximum size of a sliding window.
-}
maxWindowSize : Int
maxWindowSize =
    max_distance


type CompressionLevel
    = None
    | Fast
    | Balance
    | Best


flush : Int -> ByteArray -> Array Code
flush windowSize buffer =
    let
        codes =
            flushLoop 0 windowSize buffer (PrefixTable.new (ByteArray.length buffer)) Array.empty
    in
    codes


flushLoop : Int -> Int -> ByteArray -> PrefixTable -> Array Code -> Array Code
flushLoop i windowSize buffer prefixTable encoders =
    case PrefixTable.prefixAt i buffer of
        PrefixTable.OutOfBounds ->
            encoders

        PrefixTable.Trailing1 p1 ->
            Array.push (Literal p1) encoders

        PrefixTable.Trailing2 p1 p2 ->
            Array.push (Literal p2) (Array.push (Literal p1) encoders)

        PrefixTable.Prefix p1 key ->
            let
                -- insert the prefix into the tree, while also retrieving a previous entry
                -- if it exists. This previous entry has the same prefix, and so we'll check
                -- how long the shared prefix is there
                ( newPrefixTable, matched ) =
                    PrefixTable.insert key i prefixTable
            in
            case matched of
                Just j ->
                    let
                        distance =
                            i - j
                    in
                    if (distance - windowSize) <= 0 then
                        let
                            length =
                                3 + longestCommonPrefix (i + 3) (j + 3) buffer

                            newEncoders =
                                Array.push (Pointer length distance) encoders

                            newerPrefixTable =
                                updatePrefixTableLoop (i + 1) (i + length) buffer newPrefixTable
                        in
                        flushLoop (i + length) windowSize buffer newerPrefixTable newEncoders

                    else
                        -- found no match; encode as a literal
                        -- @optimize I tried to give the p2 and p3 values as arguments
                        -- saves 2 reads, but was slower in profiles at the time
                        flushLoop (i + 1) windowSize buffer newPrefixTable (Array.push (Literal p1) encoders)

                Nothing ->
                    -- found no match; encode as a literal
                    flushLoop (i + 1) windowSize buffer newPrefixTable (Array.push (Literal p1) encoders)


updatePrefixTableLoop : Int -> Int -> ByteArray -> PrefixTable -> PrefixTable
updatePrefixTableLoop k limit buffer prefixTable =
    if k < limit then
        case PrefixTable.prefixAt k buffer of
            PrefixTable.Prefix _ code ->
                let
                    ( newPrefixTable, _ ) =
                        PrefixTable.insert code k prefixTable
                in
                updatePrefixTableLoop (k + 1) limit buffer newPrefixTable

            _ ->
                prefixTable

    else
        prefixTable


longestCommonPrefix : Int -> Int -> ByteArray -> Int
longestCommonPrefix i j array =
    let
        remaining =
            min (max_length - 3) (ByteArray.length array - j)
    in
    longestCommonPrefixLoop i j (i + remaining) 0 array


longestCommonPrefixLoop : Int -> Int -> Int -> Int -> ByteArray -> Int
longestCommonPrefixLoop i j limit accum array =
    if i < limit then
        case ByteArray.get i array of
            Nothing ->
                accum

            Just value1 ->
                case ByteArray.get j array of
                    Nothing ->
                        accum

                    Just value2 ->
                        -- use arithmetic to force inline the compare
                        if (value1 - value2) == 0 then
                            longestCommonPrefixLoop (i + 1) (j + 1) limit (accum + 1) array

                        else
                            accum

    else
        accum
