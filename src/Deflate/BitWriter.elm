module Deflate.BitWriter exposing (BitWriter, State, empty, flush, run, writeBit, writeBits, writeEncoder)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)


run : State -> List Encoder
run state =
    List.reverse state.inner


type alias State =
    { buf : Int, end : Int, inner : List Encoder }


type Instruction
    = WriteBits Int Int
    | WriteEncoder Encoder
    | Flush


type alias BitWriter =
    State



{-

   print : BitWriter -> BitWriter
   print (BitWriter writer) =
       let
           _ =
               -- Debug.log "writer" (runHelp (List.reverse writer) { buf = 5, end = 3, inner = [] })
               ()
       in
       BitWriter writer


   append : BitWriter -> BitWriter -> BitWriter
   append (BitWriter later) (BitWriter first) =
       BitWriter (later ++ first)


   concat : List BitWriter -> BitWriter -> BitWriter
   concat writers (BitWriter first) =
       -- BitWriter (Concat writers :: first)
       List.foldl (\next accum -> accum |> append next) (BitWriter first) writers
-}


empty : State
empty =
    { buf = 0, end = 0, inner = [] }


writeBit : Bool -> State -> State
writeBit b =
    case b of
        False ->
            writeBits 1 0

        True ->
            writeBits 1 1


writeBits : Int -> Int -> State -> State
writeBits bitwidth bits state =
    flushIfNeeded (Bitwise.or state.buf (Bitwise.shiftLeftBy state.end bits)) (state.end + bitwidth) state.inner


writeEncoder : Encoder -> State -> State
writeEncoder encoder state =
    { buf = state.buf, end = state.end, inner = encoder :: state.inner }


flushIfNeeded buf end inner =
    if end >= 16 then
        { inner = Encode.unsignedInt16 LE buf :: inner
        , end = end - 16
        , buf = Bitwise.shiftRightBy 16 buf
        }

    else
        { buf = buf, end = end, inner = inner }


flush : State -> State
flush state =
    flushLoop state.buf state.end state.inner


flushLoop buf end inner =
    if end > 0 then
        flushLoop (Bitwise.shiftRightBy 8 buf) (max 0 (end - 8)) (Encode.unsignedInt8 buf :: inner)

    else
        { buf = buf, end = end, inner = inner }
