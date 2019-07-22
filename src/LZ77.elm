module LZ77 exposing
    ( encode, decode
    , Code(..)
    , encodeWithOptions, maxWindowSize
    )

{-| LZ77 finds sequences of bytes that occur multiple times, and stores them only once:

    LZ77.encode (Encode.encode (Encode.string "aaaaa"))
        --> [ Literal 97, Pointer 4 1 ]

The character `a` occurs 5 times, which is encoded as "the byte 97; go back 1 position and read 4 bytes there, putting them onto the end of the stream".
Note that the pointer tries to read 4 bytes, even though the output stream at that point only has length 1. This is fine: the elements are copied over one by one.

@docs encode, decode
@docs Code
@docs encodeWithOptions, maxWindowSize

-}

import Array exposing (Array)
import Bitwise
import ByteArray
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode exposing (Encoder)
import PrefixTable exposing (PrefixTable)


{-| Encode using the LZ77 encoding
-}
encode : Bytes -> List Code
encode buffer =
    encodeWithOptions { windowSize = maxWindowSize } buffer


{-| Encode using the LZ77 encoding, with additional options.

  - **window size**: The window size is an how far back a `Pointer` can jump. A bigger window size gives better compression, but requires more data in memory.
    That is almost never a problem nowadays though, so `encode` uses the maximum window size that LZ77 supports.

-}
encodeWithOptions : { windowSize : Int } -> Bytes -> List Code
encodeWithOptions { windowSize } buffer =
    case Decode.decode (ByteArray.decoder (Bytes.width buffer)) buffer of
        Nothing ->
            []

        Just barray ->
            flush windowSize barray


{-| Decode using the LZ77 encoding
-}
decode : List Code -> Bytes
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
    go codes Array.empty
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


flush : Int -> Array Int -> List Code
flush windowSize buffer =
    let
        codes =
            flushLoop 0 windowSize buffer (PrefixTable.new (Array.length buffer)) []

        -- remaining = Array.slice i (Array.length buffer) buffer
    in
    -- List.reverse (Array.foldl (\e accum -> Literal e :: accum) codes remaining)
    List.reverse codes


flushLoop : Int -> Int -> Array Int -> PrefixTable -> List Code -> List Code
flushLoop i windowSize buffer prefixTable encoders =
    case Array.get i buffer of
        Nothing ->
            encoders

        Just p1 ->
            case Array.get (i + 1) buffer of
                Nothing ->
                    Literal p1 :: encoders

                Just p2 ->
                    case Array.get (i + 2) buffer of
                        Nothing ->
                            Literal p2 :: Literal p1 :: encoders

                        Just p3 ->
                            let
                                -- create a prefix key (hash) for the current position
                                key : PrefixTable.Prefix
                                key =
                                    PrefixTable.createPrefix p1 p2 p3

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
                                    if distance <= windowSize then
                                        let
                                            length =
                                                3 + longestCommonPrefix (i + 3) (j + 3) buffer

                                            newEncoders =
                                                Pointer length distance :: encoders

                                            newerPrefixTable =
                                                updatePrefixTableLoop (i + 1) (i + length) buffer newPrefixTable
                                        in
                                        flushLoop (i + length) windowSize buffer newerPrefixTable newEncoders

                                    else
                                        -- found no match; encode as a literal
                                        -- @optimize I tried to give the p2 and p3 values as arguments
                                        -- saves 2 reads, but was slower in profiles at the time
                                        flushLoop (i + 1) windowSize buffer newPrefixTable (Literal p1 :: encoders)

                                Nothing ->
                                    -- found no match; encode as a literal
                                    flushLoop (i + 1) windowSize buffer newPrefixTable (Literal p1 :: encoders)


updatePrefixTableLoop : Int -> Int -> Array Int -> PrefixTable -> PrefixTable
updatePrefixTableLoop k limit buffer prefixTable =
    if k < limit then
        let
            new =
                prefix k buffer

            ( newPrefixTable, _ ) =
                PrefixTable.insert new k prefixTable
        in
        updatePrefixTableLoop (k + 1) limit buffer newPrefixTable

    else
        prefixTable


prefix : Int -> Array Int -> PrefixTable.Prefix
prefix index array =
    Maybe.map3 PrefixTable.createPrefix
        (Array.get (index + 0) array)
        (Array.get (index + 1) array)
        (Array.get (index + 2) array)
        |> Maybe.withDefault (PrefixTable.createPrefix 0 0 0)


longestCommonPrefix : Int -> Int -> Array number -> Int
longestCommonPrefix i j array =
    let
        remaining =
            min (max_length - 3) (Array.length array - j)
    in
    longestCommonPrefixLoop i j (i + remaining) 0 array


longestCommonPrefixLoop i j limit accum array =
    if i < limit then
        case Array.get i array of
            Nothing ->
                accum

            Just value1 ->
                case Array.get j array of
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
