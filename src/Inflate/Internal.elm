module Inflate.Internal exposing (HuffmanTable, Tree, buildBitsBase, buildTree, clcIndices, decodeDynamicTreeLength, decodeSymbol, decodeTrees, empty, hardcodedDistanceTable, hardcodedLengthTable, inflate, inflateBlockData, inflateBlockDataHelp, inflateUncompressedBlock, insert, sdtree, sltree, uncompress, uncompressHelp)

import Array exposing (Array)
import Bitwise
import ByteArray
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Step(..))
import Bytes.Encode as Encode
import Dict exposing (Dict)
import Experimental.ByteArray as Exp
import Inflate.BitReader as BitReader exposing (BitReader(..))
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
    BitReader.loop [] uncompressHelp
        |> BitReader.map List.reverse


uncompressHelp : List Bytes -> BitReader (Step (List Bytes) (List Bytes))
uncompressHelp output =
    let
        readTwoBits =
            BitReader.map2 (\b1 b2 -> b1 + 2 * b2) BitReader.getBit BitReader.getBit

        uncompressBlock btype =
            case btype of
                0 ->
                    -- read 5 more bits (i.e. the first byte) without reading extra bytes into the `tag`
                    BitReader.exactly 5 BitReader.getBit
                        |> BitReader.andThen (\_ -> inflateUncompressedBlock)
                        |> BitReader.map (\v -> v :: output)

                1 ->
                    -- use the static huffman trees
                    let
                        lengthSoFar =
                            List.sum (List.map Bytes.width output)
                    in
                    inflateBlockData { literal = sltree, distance = sdtree } lengthSoFar Array.empty
                        |> BitReader.map ByteArray.toBytes
                        |> BitReader.map (\v -> v :: output)

                2 ->
                    let
                        lengthSoFar =
                            List.sum (List.map Bytes.width output)
                    in
                    decodeTrees
                        |> BitReader.andThen (\( ltree, dtree ) -> inflateBlockData { literal = ltree, distance = dtree } lengthSoFar Array.empty)
                        |> BitReader.map ByteArray.toBytes
                        |> BitReader.map (\v -> v :: output)

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


type alias HuffmanTable =
    Array { bits : Int, base : Int }


readHuffmanTable : Int -> HuffmanTable -> Maybe { bits : Int, base : Int }
readHuffmanTable index table =
    Array.get index table



{-
   case Array.get index table of
       Nothing ->
           Nothing

       Just value ->
           let
               bits =
                   Bitwise.shiftRightBy 8 value

               base =
                   Bitwise.and 0xFF value
           in
           Just { bits = bits, base = base }
-}


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
    base


hardcodedLengthTable : HuffmanTable
hardcodedLengthTable =
    buildBitsBase 4 3
        -- fix a special case
        |> Array.set 28 { bits = 0, base = 258 }


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
        lengthsDict =
            lengths

        tableDict =
            Dict.foldl
                (\key value accum ->
                    if key >= offset && key < (num + offset) then
                        Dict.update value
                            (\maybeValue ->
                                case maybeValue of
                                    Nothing ->
                                        Just 1

                                    Just v ->
                                        Just (v + 1)
                            )
                            accum

                    else
                        accum
                )
                Dict.empty
                lengthsDict

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
                case Dict.get (offset + i) lengthsDict of
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
                -- ( ( newerCur, newTag ), newLen, newSum )
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
            Dict.foldl (\i _ -> insert i) empty initialLengths
    in
    BitReader.loop ( 0, initialBitSet, initialLengths ) (decodeDynamicTreeLength codeTree hlit hdist)



-- |> BitReader.map (\dict -> Dict.diff dict initialLengths)


type BitSet320
    = BitSet320 Int Int Int Int Int Int Int Int Int Int


empty =
    BitSet320 0 0 0 0 0 0 0 0 0 0


member n ((BitSet320 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10) as input) =
    if n >= 320 then
        False

    else
        let
            rem =
                n |> remainderBy 32

            bit =
                Bitwise.shiftLeftBy rem 1
        in
        case n // 32 of
            0 ->
                Bitwise.and bit b1 > 0

            1 ->
                Bitwise.and bit b1 > 1

            2 ->
                Bitwise.and bit b1 > 2

            3 ->
                Bitwise.and bit b1 > 3

            4 ->
                Bitwise.and bit b1 > 4

            5 ->
                Bitwise.and bit b1 > 5

            6 ->
                Bitwise.and bit b1 > 6

            7 ->
                Bitwise.and bit b1 > 7

            8 ->
                Bitwise.and bit b1 > 8

            9 ->
                Bitwise.and bit b1 > 9

            10 ->
                Bitwise.and bit b1 > 10

            _ ->
                False


insert n ((BitSet320 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10) as input) =
    if n >= 320 then
        input

    else
        let
            rem =
                n |> remainderBy 32

            bit =
                Bitwise.shiftLeftBy rem 1
        in
        case n // 32 of
            0 ->
                BitSet320 (Bitwise.or bit b1) b2 b3 b4 b5 b6 b7 b8 b9 b10

            1 ->
                BitSet320 b1 (Bitwise.or bit b2) b3 b4 b5 b6 b7 b8 b9 b10

            2 ->
                BitSet320 b1 b2 (Bitwise.or bit b3) b4 b5 b6 b7 b8 b9 b10

            3 ->
                BitSet320 b1 b2 b3 (Bitwise.or bit b4) b5 b6 b7 b8 b9 b10

            4 ->
                BitSet320 b1 b2 b3 b4 (Bitwise.or bit b5) b6 b7 b8 b9 b10

            5 ->
                BitSet320 b1 b2 b3 b4 b5 (Bitwise.or bit b6) b7 b8 b9 b10

            6 ->
                BitSet320 b1 b2 b3 b4 b5 b6 (Bitwise.or bit b7) b8 b9 b10

            7 ->
                BitSet320 b1 b2 b3 b4 b5 b6 b7 (Bitwise.or bit b8) b9 b10

            8 ->
                BitSet320 b1 b2 b3 b4 b5 b6 b7 b8 (Bitwise.or bit b9) b10

            9 ->
                BitSet320 b1 b2 b3 b4 b5 b6 b7 b8 b9 (Bitwise.or bit b10)

            _ ->
                input


decodeDynamicTreeLength : Tree -> Int -> Int -> ( Int, BitSet320, Dict Int Int ) -> BitReader (Step ( Int, BitSet320, Dict Int Int ) (Dict Int Int))
decodeDynamicTreeLength codeTree hlit hdist ( i, bitset, lengths ) =
    let
        copySegment : Int -> Int -> Step ( Int, BitSet320, Dict Int Int ) a
        copySegment value length =
            let
                end =
                    i + length

                go j currentBitSet accum =
                    if j < end then
                        if value /= 0 then
                            go (j + 1) (insert j currentBitSet) (Dict.insert j value accum)

                        else if member j currentBitSet then
                            -- go (j + 1) accum
                            go (j + 1) currentBitSet (Dict.remove j accum)

                        else
                            go (j + 1) currentBitSet accum

                    else
                        ( currentBitSet, accum )

                ( newBitSet, newLengths ) =
                    go i bitset lengths
            in
            Loop
                ( i + length
                , newBitSet
                , newLengths
                )
    in
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
                                |> BitReader.map (copySegment prev)

                        17 ->
                            --  repeat code length 0 for 3-10 times (read 3 bits)
                            BitReader.readBits 3 3
                                |> BitReader.map (copySegment 0)

                        18 ->
                            -- repeat code length 0 for 11-138 times (read 7 bits)
                            BitReader.readBits 7 11
                                |> BitReader.map (copySegment 0)

                        0 ->
                            -- 0 is the default of the dict; don't write it
                            -- BitReader.succeed (Loop ( i + 1, lengths ))
                            if member i bitset then
                                BitReader.succeed (Loop ( i + 1, bitset, Dict.remove i lengths ))

                            else
                                BitReader.succeed (Loop ( i + 1, bitset, lengths ))

                        _ ->
                            -- values 0-15 represent the actual code lengths
                            BitReader.succeed (Loop ( i + 1, insert i bitset, Dict.insert i sym lengths ))
                )

    else
        BitReader.succeed (Done lengths)



-- INFLATE BLOCK


inflateBlockData : { literal : Tree, distance : Tree } -> Int -> Array Int -> BitReader (Array Int)
inflateBlockData trees outputLength output =
    BitReader.loop ( outputLength, output ) (inflateBlockDataHelp trees)


inflateBlockDataHelp : { literal : Tree, distance : Tree } -> ( Int, Array Int ) -> BitReader (Step ( Int, Array Int ) (Array Int))
inflateBlockDataHelp trees ( outputLength, output ) =
    let
        lt =
            trees.literal

        dt =
            trees.distance
    in
    let
        table =
            List.tail lt.table
                |> Maybe.withDefault []
    in
    decodeSymbol table lt
        |> BitReader.andThen
            (\symbol ->
                -- check for end of block
                if symbol == 256 then
                    BitReader.succeed (Done output)

                else if symbol < 256 then
                    BitReader.succeed (Loop ( outputLength + 1, Array.push symbol output ))

                else
                    BitReader.map2 (\length offset -> Loop ( outputLength + length, copyLoop offset length offset outputLength output ))
                        (decodeLength symbol)
                        (decodeOffset outputLength dt)
            )


copyLoop : Int -> Int -> Int -> Int -> Array a -> Array a
copyLoop offs length i destLen arr =
    if (i - (offs + length)) < 0 then
        copyLoop offs
            length
            (i + 1)
            (destLen + 1)
            (let
                source =
                    i

                destination =
                    destLen
             in
             case Array.get source arr of
                Nothing ->
                    arr

                Just value ->
                    let
                        size =
                            Array.length arr
                    in
                    if (destination - size) < 0 then
                        Array.set destination value arr

                    else if (destination - size) == 0 then
                        Array.push value arr

                    else
                        arr
            )

    else
        arr


copy : Int -> Int -> Array a -> Array a
copy source destination arr =
    case Array.get source arr of
        Nothing ->
            arr

        Just value ->
            let
                size =
                    Array.length arr
            in
            if (destination - size) < 0 then
                Array.set destination value arr

            else if (destination - size) == 0 then
                Array.push value arr

            else
                arr


decodeLength : Int -> BitReader Int
decodeLength symbol =
    case readHuffmanTable (symbol - 257) hardcodedLengthTable of
        Nothing ->
            BitReader.error
                ("index out of bounds in hardcodedLengthTable: requested index "
                    ++ String.fromInt (symbol - 257)
                    ++ "but hardcodedLengthTable has length "
                    ++ String.fromInt (Array.length hardcodedLengthTable)
                )

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
                        BitReader.error
                            ("index out of bounds in hardcodedDistanceTable: requested index "
                                ++ String.fromInt distance
                                ++ "but hardcodedLengthTable has length "
                                ++ String.fromInt (Array.length hardcodedDistanceTable)
                            )

                    Just entry ->
                        BitReader.readBits entry.bits entry.base
                            {-
                               TODO is this correct?
                               We know that the blocks are independent https://www.w3.org/Graphics/PNG/RFC-1951
                               But the offset is probably still given for the whole (across blocks)
                            -}
                            |> BitReader.map (\v -> outputLength - v)
            )



-- UNCOMPRESSED BLOCK


inflateUncompressedBlock : BitReader Bytes
inflateUncompressedBlock =
    BitReader
        (\state_ ->
            let
                state =
                    BitReader.flush state_
            in
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
