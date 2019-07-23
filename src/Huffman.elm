module Huffman exposing (Code, Tree, codeFromRecord, encode, fromFrequencies, fromList, getBits, getWidth, hardcodedStaticHuffmanTree, inverseEndian, lookup, new, newCode, restoreCanonicalHuffmanCodes, setMapping, usedMaxSymbol)

import Array exposing (Array)
import Bitwise
import Deflate.BitWriter as BitWriter exposing (BitWriter)
import LengthLimitedHuffmanCodes
import List.Extra


type Code
    = Code { width : Int, bits : Int }


codeFromRecord : { width : Int, bits : Int } -> Code
codeFromRecord =
    Code


newCode : Int -> Int -> Code
newCode width bits =
    Code { width = width, bits = bits }


getWidth : Code -> Int
getWidth (Code { width }) =
    width


getBits : Code -> Int
getBits (Code { bits }) =
    bits


type Tree
    = Tree (Array Code)


fromList : List { width : Int, bits : Int } -> Tree
fromList =
    Tree << Array.fromList << List.map codeFromRecord


encode : Int -> Tree -> BitWriter -> BitWriter
encode symbol (Tree table) =
    case Array.get symbol table of
        Nothing ->
            BitWriter.writeBits 0 0

        Just (Code { width, bits }) ->
            -- BitWriter.writeBits code.width code.bits
            BitWriter.writeBits width bits


new : Int -> Tree
new n =
    Tree (Array.repeat n (codeFromRecord { width = 0, bits = 0 }))


lookup : Int -> Tree -> Maybe Code
lookup symbol (Tree array) =
    Array.get symbol array


setMapping : Int -> Code -> Tree -> Tree
setMapping symbol code (Tree array) =
    Tree (Array.set symbol (inverseEndian code) array)


inverseEndian : Code -> Code
inverseEndian (Code { width, bits }) =
    let
        inverseBits =
            inverseEndianLoop 0 width bits 0
    in
    Code { width = width, bits = inverseBits }


inverseEndianLoop i limit f t =
    if i < limit then
        inverseEndianLoop (i + 1) limit (Bitwise.shiftRightBy 1 f) (Bitwise.or (Bitwise.and f 1) (Bitwise.shiftLeftBy 1 t))

    else
        t


fromFrequencies : Array Int -> Int -> Tree
fromFrequencies symbolFrequencies maxBitWidth_ =
    let
        maxBitWidth =
            min maxBitWidth_ (calcOptimalMaxBitWidth symbolFrequencies)

        codeBitWidhts =
            LengthLimitedHuffmanCodes.calculate maxBitWidth symbolFrequencies
    in
    fromBitWidths codeBitWidhts


calcOptimalMaxBitWidth : Array Int -> Int
calcOptimalMaxBitWidth frequencies =
    let
        createHeap =
            Array.foldl createHeapFolder [] frequencies

        createHeapFolder freq heap =
            if freq > 0 then
                ( -freq, 0 ) :: heap

            else
                heap

        heapModificationLoop heap =
            case heap of
                [] ->
                    0

                [ ( _, value ) ] ->
                    max 1 value

                ( weight1, width1 ) :: ( weight2, width2 ) :: rest ->
                    -- flip comparison to get a max-heap
                    heapModificationLoop (List.sortWith (\a b -> compare b a) (( weight1 + weight2, 1 + max width1 width2 ) :: rest))
    in
    heapModificationLoop createHeap


fromBitWidths bitWidths =
    let
        symbolCount =
            bitWidths
                |> Array.indexedMap Tuple.pair
                |> Array.filter (\e -> Tuple.second e > 0)
                |> (\a -> Array.get (Array.length a - 1) a)
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0
                |> (\v -> v + 1)
    in
    restoreCanonicalHuffmanCodes bitWidths (new symbolCount)


restoreCanonicalHuffmanCodes : Array Int -> Tree -> Tree
restoreCanonicalHuffmanCodes bitWidths tree =
    let
        symbols =
            bitWidths
                |> Array.indexedMap Tuple.pair
                |> Array.filter (\( _, codeBitWidth ) -> codeBitWidth > 0)
                |> Array.toList
                |> List.Extra.stableSortWith (\( _, a ) ( _, b ) -> compare a b)

        loop ( symbol, bitWidth ) ( code, prevWidth, currentTree ) =
            let
                newBits =
                    Bitwise.shiftLeftBy (bitWidth - prevWidth) code

                nextCode =
                    Code { width = bitWidth, bits = newBits }
            in
            ( newBits + 1, bitWidth, setMapping symbol nextCode currentTree )
    in
    List.foldl loop ( 0, 0, tree ) symbols
        |> (\( _, _, x ) -> x)


usedMaxSymbol : Tree -> Maybe Int
usedMaxSymbol (Tree array) =
    array
        |> positionFromTheEnd (\(Code value) -> value.width > 0)
        |> Maybe.map (\trailingZeros -> Array.length array - 1 - trailingZeros)


positionFromTheEnd predicated array =
    let
        folder element ( index, accum ) =
            case accum of
                Just _ ->
                    ( index, accum )

                Nothing ->
                    if predicated element then
                        ( index, Just index )

                    else
                        ( index - 1, Nothing )

        finalIndex =
            Array.length array - 1
    in
    Array.foldr folder ( finalIndex, Nothing ) array
        |> Tuple.second
        |> Maybe.map (\v -> finalIndex - v)


hardcodedStaticHuffmanTree : { literal : Tree, distance : Tree }
hardcodedStaticHuffmanTree =
    { literal =
        fromList
            [ { width = 8, bits = 12 }
            , { width = 8, bits = 140 }
            , { width = 8, bits = 76 }
            , { width = 8, bits = 204 }
            , { width = 8, bits = 44 }
            , { width = 8, bits = 172 }
            , { width = 8, bits = 108 }
            , { width = 8, bits = 236 }
            , { width = 8, bits = 28 }
            , { width = 8, bits = 156 }
            , { width = 8, bits = 92 }
            , { width = 8, bits = 220 }
            , { width = 8, bits = 60 }
            , { width = 8, bits = 188 }
            , { width = 8, bits = 124 }
            , { width = 8, bits = 252 }
            , { width = 8, bits = 2 }
            , { width = 8, bits = 130 }
            , { width = 8, bits = 66 }
            , { width = 8, bits = 194 }
            , { width = 8, bits = 34 }
            , { width = 8, bits = 162 }
            , { width = 8, bits = 98 }
            , { width = 8, bits = 226 }
            , { width = 8, bits = 18 }
            , { width = 8, bits = 146 }
            , { width = 8, bits = 82 }
            , { width = 8, bits = 210 }
            , { width = 8, bits = 50 }
            , { width = 8, bits = 178 }
            , { width = 8, bits = 114 }
            , { width = 8, bits = 242 }
            , { width = 8, bits = 10 }
            , { width = 8, bits = 138 }
            , { width = 8, bits = 74 }
            , { width = 8, bits = 202 }
            , { width = 8, bits = 42 }
            , { width = 8, bits = 170 }
            , { width = 8, bits = 106 }
            , { width = 8, bits = 234 }
            , { width = 8, bits = 26 }
            , { width = 8, bits = 154 }
            , { width = 8, bits = 90 }
            , { width = 8, bits = 218 }
            , { width = 8, bits = 58 }
            , { width = 8, bits = 186 }
            , { width = 8, bits = 122 }
            , { width = 8, bits = 250 }
            , { width = 8, bits = 6 }
            , { width = 8, bits = 134 }
            , { width = 8, bits = 70 }
            , { width = 8, bits = 198 }
            , { width = 8, bits = 38 }
            , { width = 8, bits = 166 }
            , { width = 8, bits = 102 }
            , { width = 8, bits = 230 }
            , { width = 8, bits = 22 }
            , { width = 8, bits = 150 }
            , { width = 8, bits = 86 }
            , { width = 8, bits = 214 }
            , { width = 8, bits = 54 }
            , { width = 8, bits = 182 }
            , { width = 8, bits = 118 }
            , { width = 8, bits = 246 }
            , { width = 8, bits = 14 }
            , { width = 8, bits = 142 }
            , { width = 8, bits = 78 }
            , { width = 8, bits = 206 }
            , { width = 8, bits = 46 }
            , { width = 8, bits = 174 }
            , { width = 8, bits = 110 }
            , { width = 8, bits = 238 }
            , { width = 8, bits = 30 }
            , { width = 8, bits = 158 }
            , { width = 8, bits = 94 }
            , { width = 8, bits = 222 }
            , { width = 8, bits = 62 }
            , { width = 8, bits = 190 }
            , { width = 8, bits = 126 }
            , { width = 8, bits = 254 }
            , { width = 8, bits = 1 }
            , { width = 8, bits = 129 }
            , { width = 8, bits = 65 }
            , { width = 8, bits = 193 }
            , { width = 8, bits = 33 }
            , { width = 8, bits = 161 }
            , { width = 8, bits = 97 }
            , { width = 8, bits = 225 }
            , { width = 8, bits = 17 }
            , { width = 8, bits = 145 }
            , { width = 8, bits = 81 }
            , { width = 8, bits = 209 }
            , { width = 8, bits = 49 }
            , { width = 8, bits = 177 }
            , { width = 8, bits = 113 }
            , { width = 8, bits = 241 }
            , { width = 8, bits = 9 }
            , { width = 8, bits = 137 }
            , { width = 8, bits = 73 }
            , { width = 8, bits = 201 }
            , { width = 8, bits = 41 }
            , { width = 8, bits = 169 }
            , { width = 8, bits = 105 }
            , { width = 8, bits = 233 }
            , { width = 8, bits = 25 }
            , { width = 8, bits = 153 }
            , { width = 8, bits = 89 }
            , { width = 8, bits = 217 }
            , { width = 8, bits = 57 }
            , { width = 8, bits = 185 }
            , { width = 8, bits = 121 }
            , { width = 8, bits = 249 }
            , { width = 8, bits = 5 }
            , { width = 8, bits = 133 }
            , { width = 8, bits = 69 }
            , { width = 8, bits = 197 }
            , { width = 8, bits = 37 }
            , { width = 8, bits = 165 }
            , { width = 8, bits = 101 }
            , { width = 8, bits = 229 }
            , { width = 8, bits = 21 }
            , { width = 8, bits = 149 }
            , { width = 8, bits = 85 }
            , { width = 8, bits = 213 }
            , { width = 8, bits = 53 }
            , { width = 8, bits = 181 }
            , { width = 8, bits = 117 }
            , { width = 8, bits = 245 }
            , { width = 8, bits = 13 }
            , { width = 8, bits = 141 }
            , { width = 8, bits = 77 }
            , { width = 8, bits = 205 }
            , { width = 8, bits = 45 }
            , { width = 8, bits = 173 }
            , { width = 8, bits = 109 }
            , { width = 8, bits = 237 }
            , { width = 8, bits = 29 }
            , { width = 8, bits = 157 }
            , { width = 8, bits = 93 }
            , { width = 8, bits = 221 }
            , { width = 8, bits = 61 }
            , { width = 8, bits = 189 }
            , { width = 8, bits = 125 }
            , { width = 8, bits = 253 }
            , { width = 9, bits = 19 }
            , { width = 9, bits = 275 }
            , { width = 9, bits = 147 }
            , { width = 9, bits = 403 }
            , { width = 9, bits = 83 }
            , { width = 9, bits = 339 }
            , { width = 9, bits = 211 }
            , { width = 9, bits = 467 }
            , { width = 9, bits = 51 }
            , { width = 9, bits = 307 }
            , { width = 9, bits = 179 }
            , { width = 9, bits = 435 }
            , { width = 9, bits = 115 }
            , { width = 9, bits = 371 }
            , { width = 9, bits = 243 }
            , { width = 9, bits = 499 }
            , { width = 9, bits = 11 }
            , { width = 9, bits = 267 }
            , { width = 9, bits = 139 }
            , { width = 9, bits = 395 }
            , { width = 9, bits = 75 }
            , { width = 9, bits = 331 }
            , { width = 9, bits = 203 }
            , { width = 9, bits = 459 }
            , { width = 9, bits = 43 }
            , { width = 9, bits = 299 }
            , { width = 9, bits = 171 }
            , { width = 9, bits = 427 }
            , { width = 9, bits = 107 }
            , { width = 9, bits = 363 }
            , { width = 9, bits = 235 }
            , { width = 9, bits = 491 }
            , { width = 9, bits = 27 }
            , { width = 9, bits = 283 }
            , { width = 9, bits = 155 }
            , { width = 9, bits = 411 }
            , { width = 9, bits = 91 }
            , { width = 9, bits = 347 }
            , { width = 9, bits = 219 }
            , { width = 9, bits = 475 }
            , { width = 9, bits = 59 }
            , { width = 9, bits = 315 }
            , { width = 9, bits = 187 }
            , { width = 9, bits = 443 }
            , { width = 9, bits = 123 }
            , { width = 9, bits = 379 }
            , { width = 9, bits = 251 }
            , { width = 9, bits = 507 }
            , { width = 9, bits = 7 }
            , { width = 9, bits = 263 }
            , { width = 9, bits = 135 }
            , { width = 9, bits = 391 }
            , { width = 9, bits = 71 }
            , { width = 9, bits = 327 }
            , { width = 9, bits = 199 }
            , { width = 9, bits = 455 }
            , { width = 9, bits = 39 }
            , { width = 9, bits = 295 }
            , { width = 9, bits = 167 }
            , { width = 9, bits = 423 }
            , { width = 9, bits = 103 }
            , { width = 9, bits = 359 }
            , { width = 9, bits = 231 }
            , { width = 9, bits = 487 }
            , { width = 9, bits = 23 }
            , { width = 9, bits = 279 }
            , { width = 9, bits = 151 }
            , { width = 9, bits = 407 }
            , { width = 9, bits = 87 }
            , { width = 9, bits = 343 }
            , { width = 9, bits = 215 }
            , { width = 9, bits = 471 }
            , { width = 9, bits = 55 }
            , { width = 9, bits = 311 }
            , { width = 9, bits = 183 }
            , { width = 9, bits = 439 }
            , { width = 9, bits = 119 }
            , { width = 9, bits = 375 }
            , { width = 9, bits = 247 }
            , { width = 9, bits = 503 }
            , { width = 9, bits = 15 }
            , { width = 9, bits = 271 }
            , { width = 9, bits = 143 }
            , { width = 9, bits = 399 }
            , { width = 9, bits = 79 }
            , { width = 9, bits = 335 }
            , { width = 9, bits = 207 }
            , { width = 9, bits = 463 }
            , { width = 9, bits = 47 }
            , { width = 9, bits = 303 }
            , { width = 9, bits = 175 }
            , { width = 9, bits = 431 }
            , { width = 9, bits = 111 }
            , { width = 9, bits = 367 }
            , { width = 9, bits = 239 }
            , { width = 9, bits = 495 }
            , { width = 9, bits = 31 }
            , { width = 9, bits = 287 }
            , { width = 9, bits = 159 }
            , { width = 9, bits = 415 }
            , { width = 9, bits = 95 }
            , { width = 9, bits = 351 }
            , { width = 9, bits = 223 }
            , { width = 9, bits = 479 }
            , { width = 9, bits = 63 }
            , { width = 9, bits = 319 }
            , { width = 9, bits = 191 }
            , { width = 9, bits = 447 }
            , { width = 9, bits = 127 }
            , { width = 9, bits = 383 }
            , { width = 9, bits = 255 }
            , { width = 9, bits = 511 }
            , { width = 7, bits = 0 }
            , { width = 7, bits = 64 }
            , { width = 7, bits = 32 }
            , { width = 7, bits = 96 }
            , { width = 7, bits = 16 }
            , { width = 7, bits = 80 }
            , { width = 7, bits = 48 }
            , { width = 7, bits = 112 }
            , { width = 7, bits = 8 }
            , { width = 7, bits = 72 }
            , { width = 7, bits = 40 }
            , { width = 7, bits = 104 }
            , { width = 7, bits = 24 }
            , { width = 7, bits = 88 }
            , { width = 7, bits = 56 }
            , { width = 7, bits = 120 }
            , { width = 7, bits = 4 }
            , { width = 7, bits = 68 }
            , { width = 7, bits = 36 }
            , { width = 7, bits = 100 }
            , { width = 7, bits = 20 }
            , { width = 7, bits = 84 }
            , { width = 7, bits = 52 }
            , { width = 7, bits = 116 }
            , { width = 8, bits = 3 }
            , { width = 8, bits = 131 }
            , { width = 8, bits = 67 }
            , { width = 8, bits = 195 }
            , { width = 8, bits = 35 }
            , { width = 8, bits = 163 }
            , { width = 8, bits = 99 }
            , { width = 8, bits = 227 }
            ]
    , distance =
        fromList
            [ { width = 5, bits = 0 }
            , { width = 5, bits = 16 }
            , { width = 5, bits = 8 }
            , { width = 5, bits = 24 }
            , { width = 5, bits = 4 }
            , { width = 5, bits = 20 }
            , { width = 5, bits = 12 }
            , { width = 5, bits = 28 }
            , { width = 5, bits = 2 }
            , { width = 5, bits = 18 }
            , { width = 5, bits = 10 }
            , { width = 5, bits = 26 }
            , { width = 5, bits = 6 }
            , { width = 5, bits = 22 }
            , { width = 5, bits = 14 }
            , { width = 5, bits = 30 }
            , { width = 5, bits = 1 }
            , { width = 5, bits = 17 }
            , { width = 5, bits = 9 }
            , { width = 5, bits = 25 }
            , { width = 5, bits = 5 }
            , { width = 5, bits = 21 }
            , { width = 5, bits = 13 }
            , { width = 5, bits = 29 }
            , { width = 5, bits = 3 }
            , { width = 5, bits = 19 }
            , { width = 5, bits = 11 }
            , { width = 5, bits = 27 }
            , { width = 5, bits = 7 }
            , { width = 5, bits = 23 }
            ]
    }
