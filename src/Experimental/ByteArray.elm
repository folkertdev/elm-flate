module Experimental.ByteArray exposing (ByteArray, copyToBack, empty, fromBytes, fromList, get, length, push, set, toBytes, toList)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode


fromList : List Int -> ByteArray
fromList =
    List.foldl push empty


toList barray =
    List.filterMap (\i -> get i barray) (List.range 0 (length barray - 1))


type ByteArray
    = ByteArray (Array Int) Int


empty : ByteArray
empty =
    ByteArray Array.empty 0


length (ByteArray array finalSize) =
    case Array.length array * 4 of
        0 ->
            0

        l ->
            l - (4 - finalSize)


get : Int -> ByteArray -> Maybe Int
get index (ByteArray array finalSize) =
    -- @performance is caching the array length better?
    if index >= Array.length array * 4 + finalSize then
        Nothing

    else
        let
            internalIndex =
                index // 4

            offset =
                index |> remainderBy 4
        in
        case Array.get internalIndex array of
            Nothing ->
                Nothing

            Just int32 ->
                int32
                    |> Bitwise.shiftRightZfBy (8 * (3 - offset))
                    |> Bitwise.and 0xFF
                    |> Just



-- Set


mask1 =
    0xFF


mask2 =
    0xFF00


mask3 =
    0x00FF0000


mask4 =
    0xFF000000


set : Int -> Int -> ByteArray -> ByteArray
set index value ((ByteArray array finalSize) as input) =
    if index >= Array.length array * 4 + finalSize then
        input

    else
        let
            internalIndex =
                index // 4

            offset =
                index |> remainderBy 4
        in
        case Array.get internalIndex array of
            Nothing ->
                input

            Just current ->
                let
                    mask =
                        case offset of
                            0 ->
                                mask4

                            1 ->
                                mask3

                            2 ->
                                mask2

                            _ ->
                                mask1

                    cleared =
                        Bitwise.and (Bitwise.complement mask) current
                            |> Bitwise.shiftRightZfBy 0

                    shifted =
                        Bitwise.shiftLeftBy ((3 - offset) * 8) (Bitwise.and 0xFF value)

                    new =
                        Bitwise.or cleared shifted
                in
                ByteArray (Array.set internalIndex new array) finalSize


push value ((ByteArray array finalSize) as input) =
    if finalSize == 4 || Array.isEmpty array then
        ByteArray (Array.push (Bitwise.shiftLeftBy 24 value) array) 1

    else
        let
            internalIndex =
                Array.length array - 1

            offset =
                finalSize
        in
        case Array.get internalIndex array of
            Nothing ->
                input

            Just current ->
                let
                    mask =
                        case offset of
                            1 ->
                                mask4

                            2 ->
                                mask3

                            3 ->
                                mask2

                            _ ->
                                mask1

                    new =
                        Bitwise.or (Bitwise.shiftLeftBy ((3 - offset) * 8) (Bitwise.and 0xFF value)) current
                in
                ByteArray (Array.set internalIndex new array) (finalSize + 1)


copyToBack startIndex size (ByteArray array finalSize) =
    copyToBackInternal startIndex size array finalSize


copyToBackInternal : Int -> Int -> Array Int -> Int -> ByteArray
copyToBackInternal startIndex size array finalSize =
    let
        internalIndex =
            startIndex // 4

        offset =
            startIndex |> remainderBy 4
    in
    if size <= 0 then
        ByteArray array finalSize

    else
        case Array.get internalIndex array of
            Nothing ->
                ByteArray array finalSize

            Just value ->
                -- easy case
                if offset == 0 && finalSize == 4 && size >= 4 then
                    -- yay, we can copy the whole int32 in one go!
                    copyToBackInternal (startIndex + 4) (size - 4) (Array.push value array) finalSize

                else if finalSize == 4 then
                    -- must mask the read value, but can push instead of set
                    let
                        available =
                            4 - offset

                        canRead =
                            if (available - size) < 0 then
                                available

                            else
                                size

                        mask =
                            Bitwise.shiftRightZfBy ((4 - canRead) * 8) 0xFFFFFFFF

                        shiftAmount =
                            4 - (canRead + offset)

                        byte =
                            value
                                |> Bitwise.shiftRightZfBy (shiftAmount * 8)
                                |> Bitwise.and mask

                        newByte =
                            Bitwise.shiftLeftBy ((4 - canRead) * 8) byte

                        newArray =
                            Array.push newByte array
                    in
                    copyToBackInternal (startIndex + canRead)
                        (size - canRead)
                        newArray
                        canRead

                else
                    -- ugh!
                    let
                        available =
                            4 - offset

                        canRead =
                            -- min available size |> min (4 - finalSize)
                            let
                                m1 =
                                    if (available - size) < 0 then
                                        available

                                    else
                                        size

                                m2 =
                                    if m1 - (4 - finalSize) < 0 then
                                        m1

                                    else
                                        4 - finalSize
                            in
                            m2

                        mask =
                            Bitwise.shiftRightZfBy ((4 - canRead) * 8) 0xFFFFFFFF

                        shiftAmount =
                            4 - (canRead + offset)

                        byte =
                            value
                                |> Bitwise.shiftRightZfBy (shiftAmount * 8)
                                |> Bitwise.and mask
                                |> Bitwise.shiftLeftBy ((4 - (finalSize + canRead)) * 8)

                        lastIndex =
                            Array.length array - 1

                        bytesRemaining =
                            4 - offset - canRead

                        remainingBytes =
                            Bitwise.and (Bitwise.shiftRightZfBy ((4 - bytesRemaining) * 8) 0xFFFFFFFF) value
                                |> Bitwise.shiftLeftBy ((4 - bytesRemaining) * 8)
                    in
                    case Array.get lastIndex array of
                        Nothing ->
                            ByteArray array finalSize

                        Just current ->
                            let
                                newByte =
                                    Bitwise.or current byte
                            in
                            if size - canRead > 0 && size - canRead >= bytesRemaining then
                                copyToBackInternal (startIndex + canRead + bytesRemaining)
                                    (size - canRead - bytesRemaining)
                                    (Array.set lastIndex newByte array |> Array.push remainingBytes)
                                    bytesRemaining

                            else
                                copyToBackInternal (startIndex + canRead)
                                    (size - canRead)
                                    (Array.set lastIndex newByte array)
                                    (finalSize + canRead)


toBytes : ByteArray -> Bytes
toBytes (ByteArray array finalSize) =
    let
        folder element accum =
            case accum of
                [] ->
                    let
                        finalInt32 =
                            Bitwise.shiftRightZfBy ((4 - finalSize) * 8) element
                    in
                    case finalSize of
                        0 ->
                            []

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
                            [ Encode.unsignedInt32 BE element ]

                _ ->
                    Encode.unsignedInt32 BE element :: accum
    in
    Array.foldr folder [] array
        |> Encode.sequence
        |> Encode.encode


fromBytes : Bytes -> ByteArray
fromBytes buffer =
    case Decode.decode (Decode.loop ( Bytes.width buffer, Array.empty ) fromBytesHelp) buffer of
        Nothing ->
            empty

        Just ( finalSize, array ) ->
            ByteArray array finalSize


fromBytesHelp ( remaining, array ) =
    if remaining >= 20 then
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
                    ( remaining - 20
                    , array
                        |> Array.push a
                    )
            )
            (Decode.unsignedInt32 BE)

    else
        case remaining of
            0 ->
                Decode.succeed (Done ( 4, array ))

            1 ->
                Decode.unsignedInt8 |> Decode.map (\byte -> Done ( 1, Array.push (Bitwise.shiftLeftBy 24 byte) array ))

            2 ->
                Decode.unsignedInt16 BE |> Decode.map (\byte -> Done ( 2, Array.push (Bitwise.shiftLeftBy 16 byte) array ))

            _ ->
                Decode.map2 (\bytes byte -> Done ( 3, Array.push (Bitwise.or (Bitwise.shiftLeftBy 16 bytes) (Bitwise.shiftLeftBy 8 byte)) array ))
                    (Decode.unsignedInt16 BE)
                    Decode.unsignedInt8
