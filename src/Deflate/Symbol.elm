module Deflate.Symbol exposing (Symbol(..), buildBitWidthCodes, buildDynamicHuffmanCodec, buildFixedHuffmanCodec, calculateCodes, calculateRunLengths, code, dynamicFindFrequencies, encode, writeDynamicHuffmanCodec)

import Array exposing (Array)
import Deflate.BitWriter as BitWriter exposing (BitWriter)
import Huffman as Huffman


bitwidth_code_order : List Int
bitwidth_code_order =
    [ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 ]


type Symbol
    = EndOfBlock
    | Literal Int
    | Share Int Int


code : Symbol -> Int
code symbol =
    case symbol of
        Literal byte ->
            byte

        EndOfBlock ->
            256

        Share length _ ->
            if length >= 3 && length <= 10 then
                257 + length - 3

            else if length >= 11 && length <= 18 then
                265 + (length - 11) // 2

            else if length >= 19 && length <= 34 then
                269 + (length - 19) // 4

            else if length >= 35 && length <= 66 then
                273 + (length - 35) // 8

            else if length >= 67 && length <= 130 then
                277 + (length - 67) // 16

            else if length >= 131 && length <= 257 then
                281 + (length - 131) // 32

            else if length == 258 then
                285

            else
                -- unreachable
                -1


extraLength : Symbol -> Maybe ( Int, Int )
extraLength symbol =
    case symbol of
        Share length _ ->
            if (length >= 3 && length <= 10) || length == 258 then
                Nothing

            else if length >= 11 && length <= 18 then
                Just ( 1, (length - 11) |> modBy 2 )

            else if length >= 19 && length <= 34 then
                Just ( 2, (length - 19) |> modBy 4 )

            else if length >= 35 && length <= 66 then
                Just ( 3, (length - 35) |> modBy 8 )

            else if length >= 67 && length <= 130 then
                Just ( 4, (length - 67) |> modBy 16 )

            else if length >= 131 && length <= 257 then
                Just ( 5, (length - 131) |> modBy 32 )

            else
                -- unreachable
                Nothing

        _ ->
            Nothing


distance : Symbol -> Maybe ( Int, Int, Int )
distance symbol =
    case symbol of
        Share _ distance_ ->
            if distance_ <= 4 then
                Just ( distance_ - 1, 0, 0 )

            else
                let
                    go extraBits code_ base =
                        if base * 2 < distance_ then
                            go (extraBits + 1) (code_ + 2) (base * 2)

                        else
                            ( extraBits, code_, base )
                in
                let
                    ( extraBits, code_, base ) =
                        go 1 4 4

                    half =
                        base // 2

                    delta =
                        distance_ - base - 1
                in
                if distance_ <= base + half then
                    Just ( code_, extraBits, delta |> modBy half )

                else
                    Just ( code_ + 1, extraBits, delta |> modBy half )

        _ ->
            Nothing


encode : Symbol -> { literal : Huffman.Tree, distance : Huffman.Tree } -> BitWriter -> BitWriter
encode symbol htrees bitWriter =
    let
        maybeExtra =
            case extraLength symbol of
                Nothing ->
                    identity

                Just ( bits, extra ) ->
                    BitWriter.writeBits bits extra

        maybeDistance =
            case distance symbol of
                Nothing ->
                    identity

                Just ( code_, bits, extra ) ->
                    Huffman.encode code_ htrees.distance
                        >> (if bits > 0 then
                                BitWriter.writeBits bits extra

                            else
                                identity
                           )
    in
    bitWriter
        |> Huffman.encode (code symbol) htrees.literal
        |> maybeExtra
        |> maybeDistance



-- huffman codecs


type Range
    = Range Int Int


fixed_literal_or_length_code_table =
    [ ( 8, Range 0 144, 48 )
    , ( 9, Range 144 256, 400 )
    , ( 7, Range 256 280, 0 )
    , ( 8, Range 280 288, 192 )
    ]


buildFixedHuffmanCodec : { literal : Huffman.Tree, distance : Huffman.Tree }
buildFixedHuffmanCodec =
    let
        innerFolder bitwidth ( code_, symbol ) currentTree =
            Huffman.setMapping symbol (Huffman.newCode bitwidth code_) currentTree

        folder ( bitwidth, (Range start end) as symbols, codeBase ) huffmanTree =
            let
                domain =
                    List.indexedMap (\i s -> ( codeBase + 1, s )) (List.range start end)
            in
            List.foldl (innerFolder bitwidth) huffmanTree domain

        literal =
            List.foldl folder (Huffman.new 288) fixed_literal_or_length_code_table

        distance_ =
            List.range 0 (30 - 1)
                |> List.foldl (\i huffmanTree -> Huffman.setMapping i (Huffman.newCode 5 i) huffmanTree) (Huffman.new 30)
    in
    { literal = literal, distance = distance_ }


buildDynamicHuffmanCodec : Array Symbol -> { literal : Huffman.Tree, distance : Huffman.Tree }
buildDynamicHuffmanCodec symbols =
    let
        ( literalCounts, distanceCounts, emptyDistanceCount ) =
            Array.foldl dynamicFindFrequencies ( Array.repeat 286 0, Array.repeat 30 0, True ) symbols
    in
    { literal = Huffman.fromFrequencies literalCounts 15
    , distance =
        if emptyDistanceCount then
            Huffman.fromFrequencies (Array.set 0 1 distanceCounts) 15

        else
            Huffman.fromFrequencies distanceCounts 15
    }


dynamicFindFrequencies : Symbol -> ( Array Int, Array Int, Bool ) -> ( Array Int, Array Int, Bool )
dynamicFindFrequencies symbol ( literalCounts, distanceCounts, emptyDistanceCount ) =
    case distance symbol of
        Nothing ->
            ( update (code symbol) (\v -> v + 1) literalCounts, distanceCounts, emptyDistanceCount )

        Just ( d, _, _ ) ->
            ( update (code symbol) (\v -> v + 1) literalCounts
            , update d (\v -> v + 1) distanceCounts
            , False
            )


update index tagger array =
    case Array.get index array of
        Nothing ->
            array

        Just value ->
            Array.set index (tagger value) array


writeDynamicHuffmanCodec : { literal : Huffman.Tree, distance : Huffman.Tree } -> BitWriter -> BitWriter
writeDynamicHuffmanCodec trees bitWriter =
    let
        literal_code_count =
            max 257 ((Huffman.usedMaxSymbol trees.literal |> Maybe.withDefault 0) + 1)

        distance_code_count =
            max 1 ((Huffman.usedMaxSymbol trees.distance |> Maybe.withDefault 0) + 1)

        codes =
            buildBitWidthCodes literal_code_count distance_code_count { literal = trees.literal, distance = trees.distance }

        codeCounts =
            Array.foldl (\( i, _, _ ) -> update i (\v -> v + 1)) (Array.repeat 19 0) codes

        bitWidthEncoder =
            Huffman.fromFrequencies codeCounts 7

        bitwidthCodeCount =
            max 4
                (bitwidth_code_order
                    |> List.reverse
                    |> position
                        (\i ->
                            case Huffman.lookup i bitWidthEncoder of
                                Nothing ->
                                    False

                                Just value ->
                                    Huffman.getWidth value > 0
                        )
                    |> Maybe.map (\trailingZeros -> 19 - trailingZeros)
                    |> Maybe.withDefault 0
                )

        v1 : BitWriter -> BitWriter
        v1 writer =
            List.take bitwidthCodeCount bitwidth_code_order
                |> List.foldl
                    (\i current ->
                        let
                            width =
                                if Array.get i codeCounts == Just 0 then
                                    0

                                else
                                    Huffman.lookup i bitWidthEncoder
                                        |> Maybe.map Huffman.getWidth
                                        |> Maybe.withDefault 0
                        in
                        current
                            |> BitWriter.writeBits 3 width
                    )
                    writer

        v2 : BitWriter -> BitWriter
        v2 writer =
            codes
                |> Array.foldl
                    (\( code_, bits, extra ) current ->
                        if bits > 0 then
                            current
                                |> Huffman.encode code_ bitWidthEncoder
                                |> BitWriter.writeBits bits extra

                        else
                            current
                                |> Huffman.encode code_ bitWidthEncoder
                    )
                    writer
    in
    bitWriter
        |> BitWriter.writeBits 5 (literal_code_count - 257)
        |> BitWriter.writeBits 5 (distance_code_count - 1)
        |> BitWriter.writeBits 4 (bitwidthCodeCount - 4)
        |> v1
        |> v2



-- Build Bitwidth Codes


type alias RunLength =
    { value : Int, count : Int }


buildBitWidthCodes : Int -> Int -> { literal : Huffman.Tree, distance : Huffman.Tree } -> Array ( Int, Int, Int )
buildBitWidthCodes literalCodeCount distanceCodeCount trees =
    let
        runLengths =
            calculateRunLengths [ ( trees.literal, literalCodeCount ), ( trees.distance, distanceCodeCount ) ] []
    in
    calculateCodes runLengths


calculateRunLengths : List ( Huffman.Tree, Int ) -> List RunLength -> Array RunLength
calculateRunLengths lengths accum =
    case lengths of
        [] ->
            List.foldr Array.push Array.empty accum

        ( e, size ) :: rest ->
            let
                list =
                    List.range 0 (size - 1)
                        |> List.map (\x -> Huffman.lookup x e |> Maybe.map Huffman.getWidth |> Maybe.withDefault 0)
                        |> List.indexedMap Tuple.pair

                folder ( i, c ) runLengths =
                    case runLengths of
                        [] ->
                            { value = c, count = 1 } :: runLengths

                        last :: remaining ->
                            if last.value == c then
                                { value = last.value, count = last.count + 1 } :: remaining

                            else
                                { value = c, count = 1 } :: runLengths
            in
            calculateRunLengths rest (List.foldl folder accum list)


position : (a -> Bool) -> List a -> Maybe Int
position predicate elements =
    positionLoop predicate 0 elements


positionLoop predicate i elements =
    case elements of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just i

            else
                positionLoop predicate (i + 1) xs


calculateCodes : Array RunLength -> Array ( Int, Int, Int )
calculateCodes runLengths =
    let
        loop1 c codes =
            if c >= 11 then
                let
                    n =
                        min 138 c
                in
                loop1 (c - n) (Array.push ( 18, 7, n - 11 ) codes)

            else if c >= 3 then
                Array.push ( 17, 3, c - 3 ) codes

            else
                Array.append codes (Array.repeat c ( 0, 0, 0 ))

        loop2 r c codes =
            if c >= 3 then
                let
                    n =
                        min 6 c
                in
                loop2 r (c - n) (Array.push ( 16, 2, n - 3 ) codes)

            else
                Array.append codes (Array.repeat c ( r.value, 0, 0 ))

        folder r codes =
            if r.value == 0 then
                loop1 r.count codes

            else
                loop2 r (r.count - 1) (Array.push ( r.value, 0, 0 ) codes)
    in
    Array.foldl folder Array.empty runLengths
