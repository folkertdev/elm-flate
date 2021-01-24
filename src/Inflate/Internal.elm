module Inflate.Internal exposing (HuffmanTable, Tree, buildBitsBase, buildTree, clcIndices, decodeDynamicTreeLength, decodeSymbol, decodeTrees, hardcodedDistanceTable, hardcodedLengthTable, huffmanTableToList, inflate, inflateBlockData, inflateBlockDataHelp, inflateUncompressedBlock, sdtree, sltree, uncompress, uncompressHelp)

-- import ByteArray

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Step(..))
import Bytes.Encode as Encode
import Dict exposing (Dict)
import Experimental.ByteArray as ByteArray exposing (ByteArray)
import Inflate.BitReader as BitReader exposing (BitReader(..))
import Inflate.BitSet as BitSet exposing (BitSet320)
import List.Extra


inflate : Bytes -> Result String Bytes
inflate buffer =
    case BitReader.decode buffer uncompress of
        Err e ->
            Err e

        Ok values ->
            Ok (Encode.encode <| Encode.sequence (List.map Encode.bytes values))


uncompress : BitReader (List Bytes)
uncompress =
    BitReader.loop ByteArray.empty uncompressHelp
        |> BitReader.map (ByteArray.toBytes >> List.singleton)


uncompressHelp : ByteArray -> BitReader (Step ByteArray ByteArray)
uncompressHelp output =
    let
        readTwoBits =
            BitReader.map2 (\b1 b2 -> b1 + 2 * b2) BitReader.getBit BitReader.getBit

        uncompressBlock btype =
            case btype of
                0 ->
                    -- skip until the next byte
                    BitReader.skipToByteBoundary
                        |> BitReader.andThen (\_ -> inflateUncompressedBlock)
                        |> BitReader.map (\bytes -> ByteArray.appendBytes bytes output)

                1 ->
                    -- use the static huffman trees
                    inflateBlockData { literal = sltree, distance = sdtree } (ByteArray.length output) output

                2 ->
                    decodeTrees
                        |> BitReader.andThen (\( ltree, dtree ) -> inflateBlockData { literal = ltree, distance = dtree } (ByteArray.length output) output)

                _ ->
                    BitReader.error ("invalid block type: " ++ String.fromInt btype ++ " (only 0, 1 and 2 are valid block types)")

        go isFinal blockType =
            if isFinal /= 0 then
                BitReader.map Done (uncompressBlock blockType)

            else
                BitReader.map Loop (uncompressBlock blockType)
    in
    BitReader.map2 go BitReader.getBit readTwoBits
        |> BitReader.andThen identity


type HuffmanTable
    = HuffmanTable (Array { bits : Int, base : Int })


huffmanTableToList : HuffmanTable -> List { bits : Int, base : Int }
huffmanTableToList (HuffmanTable table) =
    Array.toList table


readHuffmanTable : Int -> HuffmanTable -> Maybe { bits : Int, base : Int }
readHuffmanTable index (HuffmanTable table) =
    Array.get index table


type alias Tree =
    { table : List Int, trans : Array Int }


buildBitsBase : Int -> Int -> HuffmanTable
buildBitsBase delta first =
    let
        folder bit ( sum, accum ) =
            ( sum + Bitwise.shiftLeftBy bit 1, Array.push { bits = bit, base = sum } accum )

        initializer i =
            if i < delta then
                0

            else
                (i - delta) // delta

        bits =
            Array.initialize 30 initializer

        base =
            Array.foldl folder ( first, Array.empty ) bits
                |> Tuple.second
    in
    HuffmanTable base


hardcodedLengthTable : HuffmanTable
hardcodedLengthTable =
    buildBitsBase 4 3
        -- fix a special case
        |> (\(HuffmanTable array) -> HuffmanTable (Array.set 28 { bits = 0, base = 258 } array))


hardcodedDistanceTable : HuffmanTable
hardcodedDistanceTable =
    buildBitsBase 2 1


sltree : Tree
sltree =
    { table = [ 0, 0, 0, 0, 0, 0, 0, 24, 152, 112, 0, 0, 0, 0, 0, 0 ]
    , trans =
        Array.fromList
            [ 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 280, 281, 282, 283, 284, 285, 286, 287, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255 ]
    }


sdtree : Tree
sdtree =
    { table = [ 0, 0, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , trans = Array.append (Array.fromList [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 ]) (Array.repeat (288 - 32) 0)
    }


clcIndices : List Int
clcIndices =
    [ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 ]


lengthsToDict lengths =
    Array.foldl
        (\element ( i, accum ) ->
            if element > 0 then
                ( i + 1, Dict.insert i element accum )

            else
                ( i + 1, accum )
        )
        ( 0, Dict.empty )
        lengths
        |> Tuple.second


buildTree : Dict Int Int -> Int -> Int -> Tree
buildTree lengths offset num =
    let
        tableDict =
            let
                updater maybeValue =
                    case maybeValue of
                        Nothing ->
                            Just 1

                        Just v ->
                            Just (v + 1)

                folder key value accum =
                    if key >= offset && key < (num + offset) then
                        Dict.update value updater accum

                    else
                        accum
            in
            Dict.foldl folder Dict.empty lengths

        newTable =
            let
                helper key value i array =
                    if i > key then
                        helper key value (i - 1) (0 :: array)

                    else
                        value :: array

                foldHelp key value ( i, array ) =
                    ( key - 1, helper key value i array )

                anotherGo i array =
                    if i >= 0 then
                        anotherGo (i - 1) (0 :: array)

                    else
                        array
            in
            Dict.foldr foldHelp ( 15, [] ) tableDict
                |> (\( a, b ) -> anotherGo a b)

        offsetsDict =
            Dict.foldl (\key value ( sum, dict ) -> ( sum + value, Dict.insert key sum dict )) ( 0, Dict.empty ) tableDict

        go2 i currentTranslation currentOffsets =
            if (i - num) < 0 then
                case Dict.get (offset + i) lengths of
                    Nothing ->
                        go2 (i + 1) currentTranslation currentOffsets

                    Just v ->
                        if v /= 0 then
                            case Dict.get v currentOffsets of
                                Nothing ->
                                    currentTranslation

                                Just w ->
                                    go2 (i + 1) (Array.set w i currentTranslation) (Dict.insert v (w + 1) currentOffsets)

                        else
                            go2 (i + 1) currentTranslation currentOffsets

            else
                currentTranslation

        translation2 =
            go2 0 (Array.repeat num 0) (offsetsDict |> Tuple.second)
    in
    { table = newTable, trans = translation2 }


unsafeGet i a =
    case Array.get i a of
        Nothing ->
            0

        Just v ->
            v


decodeSymbol : List Int -> Tree -> BitReader Int
decodeSymbol table tree =
    BitReader <|
        \state ->
            case
                if state.bitsAvailable < 16 then
                    BitReader.readMoreBits state

                else
                    Ok state
            of
                Err e ->
                    Err e

                Ok d ->
                    let
                        { cur, tag, bitsAvailable, sum } =
                            decodeSymbolInnerLoop table 0 d.tag d.bitsAvailable 0
                    in
                    case Array.get (sum + cur) tree.trans of
                        Nothing ->
                            Err "Index into trans tree out of bounds"

                        Just result ->
                            Ok ( result, { tag = tag, bitsAvailable = bitsAvailable, buffer = d.buffer, reserveAvailable = d.reserveAvailable, reserve = d.reserve } )


decodeSymbolInnerLoop table cur tag bitsAvailable sum =
    let
        newTag =
            Bitwise.shiftRightZfBy 1 tag
    in
    case table of
        [] ->
            -- unreachable
            { cur = cur, tag = tag, bitsAvailable = bitsAvailable, sum = sum }

        value :: rest ->
            let
                newSum =
                    sum + value

                newerCur =
                    (Bitwise.shiftLeftBy 1 cur + Bitwise.and tag 1) - value
            in
            if newerCur >= 0 then
                decodeSymbolInnerLoop rest newerCur newTag (bitsAvailable - 1) newSum

            else
                { cur = newerCur, tag = newTag, bitsAvailable = bitsAvailable - 1, sum = newSum }



-- DECODE HUFFMAN TREES


decodeTrees : BitReader ( Tree, Tree )
decodeTrees =
    BitReader.map3 cont (BitReader.readBits 5 257) (BitReader.readBits 5 1) (BitReader.readBits 4 4)
        |> BitReader.andThen identity


cont : Int -> Int -> Int -> BitReader ( Tree, Tree )
cont hlit hdist hclen =
    let
        buildTrees : Dict Int Int -> ( Tree, Tree )
        buildTrees lengths =
            ( buildTree lengths 0 hlit
            , buildTree lengths hlit hdist
            )
    in
    BitReader.exactly hclen (BitReader.readBits 3 0)
        |> BitReader.andThen (decodeTreeLengths hlit hdist hclen)
        |> BitReader.map buildTrees


decodeTreeLengths : Int -> Int -> Int -> List Int -> BitReader (Dict Int Int)
decodeTreeLengths hlit hdist hclen codeLengths =
    let
        clcs =
            List.take hclen clcIndices

        initialLengths =
            let
                go xs ys accum =
                    case xs of
                        [] ->
                            accum

                        index :: restIndex ->
                            case ys of
                                [] ->
                                    accum

                                codeLength :: restCodeLength ->
                                    if codeLength /= 0 then
                                        go restIndex restCodeLength (Dict.insert index codeLength accum)

                                    else
                                        go restIndex restCodeLength accum
            in
            go clcs codeLengths Dict.empty

        codeTree =
            buildTree initialLengths 0 19

        initialBitSet =
            Dict.foldl (\i _ -> BitSet.insert i) BitSet.empty initialLengths
    in
    BitReader.loop ( 0, initialBitSet, initialLengths ) (decodeDynamicTreeLength codeTree hlit hdist)


copySegment : Int -> Int -> BitSet320 -> Dict Int Int -> Int -> ( Int, BitSet320, Dict Int Int )
copySegment i value bitset lengths length =
    let
        end =
            i + length

        go j currentBitSet accum =
            if (j - end) < 0 then
                if value /= 0 then
                    go (j + 1) (BitSet.insert j currentBitSet) (Dict.insert j value accum)

                else if BitSet.member j currentBitSet then
                    -- overwrite a set value with 0 means removing it
                    go (j + 1) (BitSet.remove j currentBitSet) (Dict.remove j accum)

                else
                    -- if j is not set, and the value is 0, don't bother setting it
                    go (j + 1) currentBitSet accum

            else
                ( currentBitSet, accum )

        ( newBitSet, newLengths ) =
            go i bitset lengths
    in
    ( i + length
    , newBitSet
    , newLengths
    )


decodeDynamicTreeLength : Tree -> Int -> Int -> ( Int, BitSet320, Dict Int Int ) -> BitReader (Step ( Int, BitSet320, Dict Int Int ) (Dict Int Int))
decodeDynamicTreeLength codeTree hlit hdist ( i, bitset, lengths ) =
    if i < hlit + hdist then
        let
            table =
                List.tail codeTree.table
                    |> Maybe.withDefault []
        in
        decodeSymbol table codeTree
            |> BitReader.andThen
                (\sym ->
                    case sym of
                        16 ->
                            -- copy previous code length 3-6 times (read 2 bits)
                            let
                                prev =
                                    Dict.get (i - 1) lengths |> Maybe.withDefault 0
                            in
                            BitReader.readBits 2 3
                                |> BitReader.map (copySegment i prev bitset lengths >> Loop)

                        17 ->
                            --  repeat code length 0 for 3-10 times (read 3 bits)
                            BitReader.readBits 3 3
                                |> BitReader.map (copySegment i 0 bitset lengths >> Loop)

                        18 ->
                            -- repeat code length 0 for 11-138 times (read 7 bits)
                            BitReader.readBits 7 11
                                |> BitReader.map (copySegment i 0 bitset lengths >> Loop)

                        0 ->
                            -- 0 is the default of the dict; don't write it
                            -- BitReader.succeed (Loop ( i + 1, lengths ))
                            if BitSet.member i bitset then
                                BitReader.succeed (Loop ( i + 1, bitset, Dict.remove i lengths ))

                            else
                                BitReader.succeed (Loop ( i + 1, bitset, lengths ))

                        _ ->
                            -- values 0-15 represent the actual code lengths
                            BitReader.succeed (Loop ( i + 1, BitSet.insert i bitset, Dict.insert i sym lengths ))
                )

    else
        BitReader.succeed (Done lengths)



-- INFLATE BLOCK


inflateBlockData : { literal : Tree, distance : Tree } -> Int -> ByteArray -> BitReader ByteArray
inflateBlockData trees outputLength output =
    BitReader.loop ( outputLength, output ) (inflateBlockDataHelp trees)


inflateBlockDataHelp : { literal : Tree, distance : Tree } -> ( Int, ByteArray ) -> BitReader (Step ( Int, ByteArray ) ByteArray)
inflateBlockDataHelp trees ( outputLength, output ) =
    let
        table =
            List.tail trees.literal.table
                |> Maybe.withDefault []
    in
    decodeSymbol table trees.literal
        |> BitReader.andThen
            (\symbol ->
                -- check for end of block
                if symbol == 256 then
                    BitReader.succeed (Done output)

                else if symbol < 256 then
                    BitReader.succeed (Loop ( outputLength + 1, ByteArray.push symbol output ))

                else
                    BitReader.map2 (\length offset -> Loop ( outputLength + length, ByteArray.copyToBack offset length output ))
                        (decodeLength symbol)
                        (decodeOffset outputLength trees.distance)
            )


decodeLength : Int -> BitReader Int
decodeLength symbol =
    case readHuffmanTable (symbol - 257) hardcodedLengthTable of
        Nothing ->
            BitReader.error <|
                let
                    (HuffmanTable internal) =
                        hardcodedDistanceTable
                in
                "index out of bounds in hardcodedLengthTable: requested index "
                    ++ String.fromInt (symbol - 257)
                    ++ "but hardcodedLengthTable has length "
                    ++ String.fromInt (Array.length internal)

        Just entry ->
            BitReader.readBits entry.bits entry.base


decodeOffset : Int -> Tree -> BitReader Int
decodeOffset outputLength dt =
    let
        table_ =
            List.tail dt.table
                |> Maybe.withDefault []
    in
    decodeSymbol table_ dt
        |> BitReader.andThen
            (\distance ->
                case readHuffmanTable distance hardcodedDistanceTable of
                    Nothing ->
                        BitReader.error <|
                            let
                                (HuffmanTable internal) =
                                    hardcodedDistanceTable
                            in
                            "index out of bounds in hardcodedDistanceTable: requested index "
                                ++ String.fromInt distance
                                ++ "but hardcodedLengthTable has length "
                                ++ String.fromInt (Array.length internal)

                    Just entry ->
                        BitReader.readBits entry.bits entry.base
                            |> BitReader.map (\v -> outputLength - v)
            )



-- UNCOMPRESSED BLOCK


inflateUncompressedBlock : BitReader Bytes
inflateUncompressedBlock =
    BitReader
        (\state ->
            -- assumption: we are at a byte boundary
            case Decode.decode (uncompressedBlockDecoder (Bytes.width state.buffer)) state.buffer of
                Nothing ->
                    Err "inflateUncompressedBlock: ran out of bounds"

                Just ( block, newBuffer ) ->
                    Ok ( block, { state | buffer = newBuffer } )
        )


uncompressedBlockDecoder : Int -> Decode.Decoder ( Bytes, Bytes )
uncompressedBlockDecoder bufferWidth =
    let
        decodeLengths =
            Decode.map2 Tuple.pair (Decode.unsignedInt16 LE) (Decode.unsignedInt16 LE)
    in
    decodeLengths
        |> Decode.andThen
            (\( length, invlength ) ->
                -- invlength has to be the complement of length for this block to be valid
                -- like a small simple checksum
                if length /= Bitwise.and (Bitwise.complement invlength) 0xFFFF then
                    Decode.fail

                else
                    let
                        remainingSize =
                            bufferWidth - 4 - length
                    in
                    Decode.map2 Tuple.pair (Decode.bytes length) (Decode.bytes remainingSize)
            )
