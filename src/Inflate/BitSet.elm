module Inflate.BitSet exposing (BitSet320, empty, insert, member, remove)

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
            bit =
                Bitwise.shiftLeftBy (remainderBy 32 n) 1
        in
        case n // 32 of
            0 ->
                Bitwise.and bit b1 > 0

            1 ->
                Bitwise.and bit b2 > 0

            2 ->
                Bitwise.and bit b3 > 0

            3 ->
                Bitwise.and bit b4 > 0

            4 ->
                Bitwise.and bit b5 > 0

            5 ->
                Bitwise.and bit b6 > 0

            6 ->
                Bitwise.and bit b7 > 0

            7 ->
                Bitwise.and bit b8 > 0

            8 ->
                Bitwise.and bit b9 > 0

            9 ->
                Bitwise.and bit b10 > 0

            _ ->
                False


insert : Int -> BitSet320 -> BitSet320
insert n ((BitSet320 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10) as input) =
    if n >= 320 then
        input

    else
        let
            bit =
                Bitwise.shiftLeftBy (remainderBy 32 n) 1
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


remove : Int -> BitSet320 -> BitSet320
remove n ((BitSet320 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10) as input) =
    if n >= 320 then
        input

    else
        let
            bit =
                Bitwise.shiftLeftBy (remainderBy 32 n) 1
                    |> Bitwise.complement
        in
        case n // 32 of
            0 ->
                BitSet320 (Bitwise.and bit b1) b2 b3 b4 b5 b6 b7 b8 b9 b10

            1 ->
                BitSet320 b1 (Bitwise.and bit b2) b3 b4 b5 b6 b7 b8 b9 b10

            2 ->
                BitSet320 b1 b2 (Bitwise.and bit b3) b4 b5 b6 b7 b8 b9 b10

            3 ->
                BitSet320 b1 b2 b3 (Bitwise.and bit b4) b5 b6 b7 b8 b9 b10

            4 ->
                BitSet320 b1 b2 b3 b4 (Bitwise.and bit b5) b6 b7 b8 b9 b10

            5 ->
                BitSet320 b1 b2 b3 b4 b5 (Bitwise.and bit b6) b7 b8 b9 b10

            6 ->
                BitSet320 b1 b2 b3 b4 b5 b6 (Bitwise.and bit b7) b8 b9 b10

            7 ->
                BitSet320 b1 b2 b3 b4 b5 b6 b7 (Bitwise.and bit b8) b9 b10

            8 ->
                BitSet320 b1 b2 b3 b4 b5 b6 b7 b8 (Bitwise.and bit b9) b10

            9 ->
                BitSet320 b1 b2 b3 b4 b5 b6 b7 b8 b9 (Bitwise.and bit b10)

            _ ->
                input
