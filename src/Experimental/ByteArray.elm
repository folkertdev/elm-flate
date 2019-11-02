module Experimental.ByteArray exposing
    ( ByteArray
    , empty
    , length, get, getInt32
    , set, push, copyToBack
    , fromList, toList
    , fromBytes, toBytes, appendBytes
    , foldl, foldr
    , longestCommonPrefix
    )

{-| A `ByteArray` type

The idea of this array type is to store 4 bytes in one integer value. That means storage is much more compact, and array operations are faster because there are fewer elements to go through.


# ByteArrays

@docs ByteArray


# Creation

@docs empty, initialize, repeat


# Query

@docs isEmpty, length, get, getInt32


# Manipulate

@docs set, update, push, copyToBack


# Conversion

@docs fromList, toList
@docs fromBytes, toBytes, appendBytes


# Transform

@docs foldl, foldr

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode


type ByteArray
    = ByteArray (Array Int) Int Int


fromList : List Int -> ByteArray
fromList =
    List.foldl push empty


toList : ByteArray -> List Int
toList barray =
    foldr (::) [] barray


appendBytes : Bytes -> ByteArray -> ByteArray
appendBytes bytes ((ByteArray array finalSize finalBytes) as barray) =
    let
        decoder =
            Decode.loop ( Bytes.width bytes, barray ) appendBytesHelp
    in
    case Decode.decode decoder bytes of
        Just v ->
            v

        Nothing ->
            barray


appendBytesHelp ( remaining, (ByteArray array finalSize finalBytes) as bytearray ) =
    if remaining >= 4 then
        Decode.unsignedInt32 BE
            |> Decode.map
                (\new ->
                    Decode.Loop ( remaining - 4, pushMany 4 new bytearray )
                )

    else if remaining >= 1 then
        Decode.unsignedInt8
            |> Decode.map
                (\new ->
                    Decode.Loop ( remaining - 1, push new bytearray )
                )

    else
        Decode.succeed (Decode.Done bytearray)


pushByte : Int -> Int -> Int
pushByte small big =
    Bitwise.or (Bitwise.shiftLeftBy 8 big) small


foldl : (Int -> b -> b) -> b -> ByteArray -> b
foldl folder initial (ByteArray array finalSize finalBytes) =
    let
        internalFolder int32 accum =
            let
                b1 =
                    Bitwise.shiftRightZfBy 24 int32

                b2 =
                    Bitwise.shiftRightZfBy 16 int32 |> Bitwise.and 0xFF

                b3 =
                    Bitwise.shiftRightZfBy 8 int32 |> Bitwise.and 0xFF

                b4 =
                    int32 |> Bitwise.and 0xFF
            in
            accum
                |> folder b1
                |> folder b2
                |> folder b3
                |> folder b4
    in
    Array.foldl internalFolder initial array
        |> (\afterArray ->
                let
                    b1 =
                        Bitwise.shiftRightZfBy 24 finalBytes

                    b2 =
                        Bitwise.shiftRightZfBy 16 finalBytes |> Bitwise.and 0xFF

                    b3 =
                        Bitwise.shiftRightZfBy 8 finalBytes |> Bitwise.and 0xFF

                    b4 =
                        finalBytes |> Bitwise.and 0xFF
                in
                case finalSize of
                    1 ->
                        afterArray |> folder b1

                    2 ->
                        afterArray |> folder b1 |> folder b2

                    3 ->
                        afterArray |> folder b1 |> folder b2 |> folder b3

                    4 ->
                        afterArray |> folder b1 |> folder b2 |> folder b3 |> folder b4

                    _ ->
                        afterArray
           )


foldr : (Int -> b -> b) -> b -> ByteArray -> b
foldr folder initial (ByteArray array finalSize finalBytes) =
    let
        internalFolder int32 accum =
            let
                b1 =
                    Bitwise.shiftRightZfBy 24 int32

                b2 =
                    Bitwise.shiftRightZfBy 16 int32 |> Bitwise.and 0xFF

                b3 =
                    Bitwise.shiftRightZfBy 8 int32 |> Bitwise.and 0xFF

                b4 =
                    int32 |> Bitwise.and 0xFF
            in
            accum
                |> folder b4
                |> folder b3
                |> folder b2
                |> folder b1

        afterFinal =
            let
                b1 =
                    Bitwise.shiftRightZfBy 24 finalBytes

                b2 =
                    Bitwise.shiftRightZfBy 16 finalBytes |> Bitwise.and 0xFF

                b3 =
                    Bitwise.shiftRightZfBy 8 finalBytes |> Bitwise.and 0xFF

                b4 =
                    finalBytes |> Bitwise.and 0xFF
            in
            case finalSize of
                1 ->
                    initial |> folder b1

                2 ->
                    initial |> folder b2 |> folder b1

                3 ->
                    initial |> folder b3 |> folder b2 |> folder b1

                4 ->
                    initial |> folder b4 |> folder b3 |> folder b2 |> folder b1

                _ ->
                    initial
    in
    Array.foldr internalFolder afterFinal array


empty : ByteArray
empty =
    ByteArray Array.empty 0 0


initialize : Int -> (Int -> Int) -> ByteArray
initialize size initializer =
    empty


repeat : Int -> Int -> ByteArray
repeat size value_ =
    let
        value =
            Bitwise.and 0xFF value_

        asInt32 =
            Bitwise.or
                (Bitwise.or (Bitwise.shiftLeftBy 24 value) (Bitwise.shiftLeftBy 16 value))
                (Bitwise.or (Bitwise.shiftLeftBy 8 value) value)

        ( remainderSize, remainderBytes ) =
            case size |> remainderBy 4 of
                0 ->
                    ( 0, 0 )

                1 ->
                    ( 1, value )

                2 ->
                    ( 2, Bitwise.or (Bitwise.shiftLeftBy 8 value) value )

                3 ->
                    ( 3, Bitwise.or (Bitwise.shiftLeftBy 16 value) (Bitwise.or (Bitwise.shiftLeftBy 8 value) value) )

                _ ->
                    ( 4
                    , Bitwise.or
                        (Bitwise.or (Bitwise.shiftLeftBy 24 value) (Bitwise.shiftLeftBy 16 value))
                        (Bitwise.or (Bitwise.shiftLeftBy 8 value) value)
                    )
    in
    ByteArray (Array.repeat (size // 4) asInt32) remainderSize remainderBytes


length : ByteArray -> Int
length (ByteArray array finalSize finalBytes) =
    case Array.length array * 4 of
        0 ->
            finalSize

        l ->
            l + finalSize


get : Int -> ByteArray -> Maybe Int
get index (ByteArray array finalSize finalBytes) =
    let
        offset =
            index |> remainderBy 4
    in
    if index >= Array.length array * 4 + finalSize then
        Nothing

    else if index >= Array.length array * 4 then
        -- in the final int32
        finalBytes
            |> Bitwise.shiftRightZfBy (8 * (3 - offset))
            |> Bitwise.and 0xFF
            |> Just

    else
        let
            internalIndex =
                index // 4
        in
        case Array.get internalIndex array of
            Nothing ->
                Nothing

            Just int32 ->
                int32
                    |> Bitwise.shiftRightZfBy (8 * (3 - offset))
                    |> Bitwise.and 0xFF
                    |> Just


getInt32 : Int -> ByteArray -> Maybe Int
getInt32 index (ByteArray array _ finalBytes) =
    let
        size =
            Array.length array
    in
    if (index - size) == 0 then
        Just finalBytes

    else
        Array.get index array



-- Set


set : Int -> Int -> ByteArray -> ByteArray
set index value ((ByteArray array finalSize finalBytes) as input) =
    let
        offset =
            index |> remainderBy 4
    in
    if index >= Array.length array * 4 + finalSize then
        input

    else if index > Array.length array * 4 then
        let
            mask =
                Bitwise.shiftRightZfBy (offset * 8) 0xFF000000

            cleared =
                Bitwise.and (Bitwise.complement mask) finalBytes
                    |> Bitwise.shiftRightZfBy 0

            shifted =
                Bitwise.shiftLeftBy ((3 - offset) * 8) (Bitwise.and 0xFF value)

            new =
                Bitwise.or cleared shifted
        in
        ByteArray array finalSize new

    else
        let
            internalIndex =
                index // 4
        in
        case Array.get internalIndex array of
            Nothing ->
                input

            Just current ->
                let
                    mask =
                        Bitwise.shiftRightZfBy (offset * 8) 0xFF000000

                    cleared =
                        Bitwise.and (Bitwise.complement mask) current
                            |> Bitwise.shiftRightZfBy 0

                    shifted =
                        Bitwise.shiftLeftBy ((3 - offset) * 8) (Bitwise.and 0xFF value)

                    new =
                        Bitwise.or cleared shifted
                in
                ByteArray (Array.set internalIndex new array) finalSize finalBytes



-- update


update : Int -> (Int -> Int) -> ByteArray -> ByteArray
update index f ((ByteArray array finalSize finalBytes) as input) =
    let
        offset =
            index |> remainderBy 4
    in
    if index >= Array.length array * 4 + finalSize then
        input

    else if index >= Array.length array * 4 then
        -- in the final int32
        let
            value =
                finalBytes
                    |> Bitwise.shiftRightZfBy (8 * (3 - offset))
                    |> Bitwise.and 0xFF
                    |> f

            mask =
                Bitwise.shiftRightZfBy (offset * 8) 0xFF000000

            cleared =
                Bitwise.and (Bitwise.complement mask) finalBytes
                    |> Bitwise.shiftRightZfBy 0

            shifted =
                Bitwise.shiftLeftBy ((3 - offset) * 8) (Bitwise.and 0xFF value)

            new =
                Bitwise.or cleared shifted
        in
        ByteArray array finalSize new

    else
        let
            internalIndex =
                index // 4
        in
        case Array.get internalIndex array of
            Nothing ->
                input

            Just current ->
                let
                    value =
                        current
                            |> Bitwise.shiftRightZfBy (8 * (3 - offset))
                            |> Bitwise.and 0xFF
                            |> f

                    mask =
                        Bitwise.shiftRightZfBy (offset * 8) 0xFF000000

                    cleared =
                        Bitwise.and (Bitwise.complement mask) current
                            |> Bitwise.shiftRightZfBy 0

                    shifted =
                        Bitwise.shiftLeftBy ((3 - offset) * 8) (Bitwise.and 0xFF value)

                    new =
                        Bitwise.or cleared shifted
                in
                ByteArray (Array.set internalIndex new array) finalSize finalBytes


push value ((ByteArray array finalSize finalBytes) as input) =
    if finalSize == 4 then
        ByteArray (Array.push finalBytes array) 1 (Bitwise.shiftLeftBy 24 value)

    else if finalSize == 0 then
        ByteArray array 1 (Bitwise.shiftLeftBy 24 value)

    else
        let
            internalIndex =
                Array.length array - 1

            offset =
                finalSize
        in
        let
            mask =
                Bitwise.shiftRightZfBy (offset * 8) 0xFF000000

            new =
                Bitwise.or (Bitwise.shiftLeftBy ((3 - offset) * 8) (Bitwise.and 0xFF value)) finalBytes
        in
        ByteArray array (finalSize + 1) new


{-| Push up to 4 bytes at once.
-}
pushMany : Int -> Int -> ByteArray -> ByteArray
pushMany nbytes value_ ((ByteArray array finalSize finalBytes) as input) =
    let
        value =
            if nbytes == 4 then
                value_

            else
                Bitwise.and (Bitwise.shiftLeftBy (nbytes * 8) 1 - 1) value_
    in
    if nbytes == 0 then
        input

    else if finalSize == 4 then
        ByteArray (Array.push finalBytes array) nbytes (Bitwise.shiftLeftBy ((4 - nbytes) * 8) value)

    else if finalSize == 0 then
        ByteArray array nbytes (Bitwise.shiftLeftBy ((4 - nbytes) * 8) value)

    else
        let
            freeSpace =
                4 - finalSize
        in
        if nbytes > freeSpace then
            let
                -- fill current final bytes completely, then push it and make new final bytes
                bytesLeftOver =
                    (finalSize + nbytes) - 4

                -- shift off the bytes that don't fit
                forFinal =
                    Bitwise.shiftRightZfBy (bytesLeftOver * 8) value

                newFinal =
                    Bitwise.or finalBytes forFinal

                amount =
                    (8 - finalSize - nbytes) * 8

                forNextFinal =
                    Bitwise.and (Bitwise.shiftLeftBy (bytesLeftOver * 8) 1 - 1) value
                        |> Bitwise.shiftLeftBy amount
            in
            ByteArray (Array.push newFinal array) (nbytes - freeSpace) forNextFinal

        else
            let
                amount =
                    (4 - (finalSize + nbytes)) * 8

                forFinal =
                    Bitwise.shiftLeftBy amount value

                newFinal =
                    Bitwise.or finalBytes forFinal
            in
            ByteArray array (finalSize + nbytes) newFinal


copyToBack startIndex size (ByteArray array finalSize finalBytes) =
    copyToBackInternal startIndex size array finalSize finalBytes


copyToBackInternal : Int -> Int -> Array Int -> Int -> Int -> ByteArray
copyToBackInternal startIndex size array finalSize finalBytes =
    let
        internalIndex =
            startIndex // 4

        offset =
            startIndex |> remainderBy 4
    in
    if size <= 0 then
        ByteArray array finalSize finalBytes

    else if startIndex + 4 >= ((Array.length array - 1) * 4 + finalSize) then
        -- the slow version
        case get startIndex (ByteArray array finalSize finalBytes) of
            Nothing ->
                ByteArray array finalSize finalBytes

            Just value ->
                let
                    (ByteArray newArray newFinalSize newFinalBytes) =
                        push value (ByteArray array finalSize finalBytes)
                in
                copyToBackInternal (startIndex + 1) (size - 1) newArray newFinalSize newFinalBytes

    else
        case Array.get internalIndex array of
            Nothing ->
                ByteArray array finalSize finalBytes

            Just value ->
                let
                    maskedFront =
                        Bitwise.shiftLeftBy (8 * offset) value

                    maskedBack =
                        if 4 - offset > size then
                            let
                                bytesWeHave =
                                    (3 - offset) + 1

                                bytesWeNeedToRemove =
                                    4 - size
                            in
                            Bitwise.shiftRightBy (bytesWeNeedToRemove * 8) maskedFront

                        else
                            Bitwise.shiftRightBy (offset * 8) maskedFront

                    written =
                        min (4 - offset) size

                    (ByteArray x y z) =
                        pushMany written maskedBack (ByteArray array finalSize finalBytes)
                in
                copyToBackInternal (startIndex + written) (size - written) x y z


toBytes : ByteArray -> Bytes
toBytes (ByteArray array finalSize finalBytes) =
    let
        initial =
            let
                finalInt32 =
                    Bitwise.shiftRightZfBy ((4 - finalSize) * 8) finalBytes
            in
            case finalSize of
                4 ->
                    [ Encode.unsignedInt32 BE finalBytes ]

                1 ->
                    [ Encode.unsignedInt8 finalInt32
                    ]

                2 ->
                    [ Encode.unsignedInt16 BE finalInt32
                    ]

                3 ->
                    [ Encode.unsignedInt16 BE (Bitwise.shiftRightBy 8 finalInt32)
                    , Encode.unsignedInt8 (Bitwise.and 0xFF finalInt32)
                    ]

                _ ->
                    []

        folder element accum =
            Encode.unsignedInt32 BE element :: accum
    in
    Array.foldr folder initial array
        |> Encode.sequence
        |> Encode.encode


{-| The fastest conversion to bytes I've been able to find.

`Decode.map5` is the largest map implemented, so we use that. It turns out that pushes are so expensive
(partly due to code generation) that an append (and all the allocation etc. that comes with it) is faster.

-}
fromBytes : Bytes -> ByteArray
fromBytes buffer =
    case Decode.decode (Decode.loop ( Bytes.width buffer, Array.empty ) fromBytesHelp) buffer of
        Nothing ->
            empty

        Just ( finalSize, finalBytes, array ) ->
            ByteArray array finalSize finalBytes


fromBytesHelp ( remaining, array ) =
    if remaining >= 40 then
        -- yes, I profiled this and it is faster
        Decode.map5
            (\a b c d e ->
                Decode.map5
                    (\f g h i j ->
                        Loop
                            ( remaining - 40
                            , Array.append array (Array.fromList [ a, b, c, d, e, f, g, h, i, j ])
                            )
                    )
                    (Decode.unsignedInt32 BE)
                    (Decode.unsignedInt32 BE)
                    (Decode.unsignedInt32 BE)
                    (Decode.unsignedInt32 BE)
                    (Decode.unsignedInt32 BE)
            )
            (Decode.unsignedInt32 BE)
            (Decode.unsignedInt32 BE)
            (Decode.unsignedInt32 BE)
            (Decode.unsignedInt32 BE)
            (Decode.unsignedInt32 BE)
            |> Decode.andThen identity

    else if remaining >= 20 then
        Decode.map5
            (\a b c d e ->
                Loop
                    ( remaining - 20
                    , array
                        |> Array.push a
                        |> Array.push b
                        |> Array.push c
                        |> Array.push d
                        |> Array.push e
                    )
            )
            (Decode.unsignedInt32 BE)
            (Decode.unsignedInt32 BE)
            (Decode.unsignedInt32 BE)
            (Decode.unsignedInt32 BE)
            (Decode.unsignedInt32 BE)

    else if remaining >= 4 then
        Decode.map
            (\a ->
                Loop
                    ( remaining - 4
                    , array
                        |> Array.push a
                    )
            )
            (Decode.unsignedInt32 BE)

    else
        case remaining of
            0 ->
                Decode.succeed (Done ( 0, 0, array ))

            1 ->
                Decode.unsignedInt8 |> Decode.map (\byte -> Done ( 1, Bitwise.shiftLeftBy 24 byte, array ))

            2 ->
                Decode.unsignedInt16 BE |> Decode.map (\byte -> Done ( 2, Bitwise.shiftLeftBy 16 byte, array ))

            _ ->
                Decode.map2 (\bytes byte -> Done ( 3, Bitwise.or (Bitwise.shiftLeftBy 16 bytes) (Bitwise.shiftLeftBy 8 byte), array ))
                    (Decode.unsignedInt16 BE)
                    Decode.unsignedInt8


{-| Find the longest common prefix
-}
longestCommonPrefix : Int -> Int -> Int -> ByteArray -> Int
longestCommonPrefix max_length i j array =
    let
        remaining =
            min (max_length - 3) (length array - j)
    in
    longestCommonPrefixLoop i j (i + remaining) 0 array


longestCommonPrefixLoop : Int -> Int -> Int -> Int -> ByteArray -> Int
longestCommonPrefixLoop i j limit accum array =
    if i < limit then
        case get i array of
            Nothing ->
                accum

            Just value1 ->
                case get j array of
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


{-| minimize the number of gets. not sure if this is correct at the moment. most of the runs that lz77 finds are short anyway.
From my tests this is not actually the bottleneck: the prefixtable is.
-}
fasterCommonPrefixLoop max_length i j prefixLength value1 value2 cache1 cache2 offset1 offset2 barray =
    if value1 == value2 && max_length + 4 <= max_length then
        case getInt32 (i // 4) barray of
            Nothing ->
                let
                    remaining =
                        min (max_length - 3) (length barray - j)
                in
                longestCommonPrefixLoop i j (i + remaining) prefixLength barray

            Just nextValue1 ->
                case getInt32 (j // 4) barray of
                    Nothing ->
                        let
                            remaining =
                                min (max_length - 3) (length barray - j)
                        in
                        longestCommonPrefixLoop i j (i + remaining) prefixLength barray

                    Just nextValue2 ->
                        let
                            newValue1 =
                                Bitwise.or cache1 (Bitwise.shiftRightBy ((4 - offset1) * 8) nextValue1)

                            newCache1 =
                                Bitwise.shiftLeftBy (offset1 * 8) nextValue1

                            newValue2 =
                                Bitwise.or cache2 (Bitwise.shiftRightBy ((4 - offset2) * 8) nextValue2)

                            newCache2 =
                                Bitwise.shiftLeftBy (offset2 * 8) nextValue2
                        in
                        fasterCommonPrefixLoop max_length (i + 4) (j + 4) (prefixLength + 4) newValue1 newValue2 newCache1 newCache2 offset1 offset2 barray

    else
        let
            remaining =
                min (max_length - 3) (length barray - j)
        in
        longestCommonPrefixLoop i j (i + remaining) prefixLength barray
