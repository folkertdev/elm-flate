module Inflate.BitSet exposing (BitSet320, empty, insert, member)

{-| A BitSet to speed up array operations

Dynamic Huffman Compression uses a dictionary with 320 elements, but most are 0.
This BitSet makes checking for membership MUCH faster, and only then do we actually perform Dict operations.

-}

import Bitwise


type BitSet320
    = BitSet320 Int Int Int Int Int Int Int Int Int Int


empty : BitSet320
empty =
    BitSet320 0 0 0 0 0 0 0 0 0 0


member : Int -> BitSet320 -> Bool
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


insert : Int -> BitSet320 -> BitSet320
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
