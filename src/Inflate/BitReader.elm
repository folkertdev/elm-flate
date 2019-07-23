module Inflate.BitReader exposing (BitReader(..), State, Tree, andMap, andThen, chunksOf, decode, embed, error, exactly, exactlyBytes, fillWindow, getBit, loop, map, map2, map3, map4, readBits, runDecoder, shiftOffTag, shiftOntoTag, succeed)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode


type alias Tree =
    { table : Array Int, trans : Array Int }


type alias State =
    { tag : Int, bitsAvailable : Int, buffer : Bytes }


decode : Bytes -> BitReader a -> Result String a
decode bytes (BitReader reader) =
    let
        initialState =
            { buffer = bytes
            , tag = 0
            , bitsAvailable = 0
            }
    in
    reader initialState
        |> Result.map Tuple.first


exactlyBytes : Int -> Decoder a -> Decoder (List a)
exactlyBytes tableCount decoder =
    let
        helper ( n, xs ) =
            if n <= 0 then
                Decode.succeed (Done (List.reverse xs))

            else
                Decode.map (\x -> Loop ( n - 1, x :: xs )) decoder
    in
    Decode.loop ( tableCount, [] ) helper


exactly : Int -> BitReader a -> BitReader (List a)
exactly tableCount decoder =
    let
        helper ( n, xs ) =
            if n <= 0 then
                succeed (Done (List.reverse xs))

            else
                map (\x -> Loop ( n - 1, x :: xs )) decoder
    in
    loop ( tableCount, [] ) helper



{-
   exactly : Int -> Decoder a -> Decoder (List a)
   exactly tableCount decoder =
       let
           helper ( n, xs ) =
               if n <= 0 then
                   Decode.succeed (Done (List.reverse xs))

               else
                   Decode.map (\x -> Loop ( n - 1, x :: xs )) decoder
       in
       Decode.loop ( tableCount, [] ) helper
-}


chunksOf : Int -> Bytes -> List Bytes
chunksOf bytesPerChunk buffer =
    let
        fullChunks =
            Bytes.width buffer // bytesPerChunk

        finalChunkSize =
            Bytes.width buffer |> modBy bytesPerChunk

        boundaries =
            List.map (\i -> i * bytesPerChunk) (List.range 0 (Bytes.width buffer // bytesPerChunk)) ++ [ Bytes.width buffer ]

        decoder =
            exactlyBytes fullChunks (Decode.bytes bytesPerChunk)
                |> Decode.andThen
                    (\full ->
                        Decode.bytes finalChunkSize
                            |> Decode.map (\final -> full ++ [ final ])
                    )
    in
    Decode.decode decoder buffer
        |> Maybe.withDefault []


type BitReader b
    = BitReader (State -> Result String ( b, State ))


succeed : a -> BitReader a
succeed x =
    BitReader (\s -> Ok ( x, s ))


error : String -> BitReader a
error e =
    BitReader (\s -> Err e)


embed : (State -> Result String ( b, State )) -> BitReader b
embed =
    BitReader


map : (a -> b) -> BitReader a -> BitReader b
map f (BitReader g) =
    BitReader
        (\s ->
            case g s of
                Ok ( value, newState ) ->
                    Ok ( f value, newState )

                Err e ->
                    Err e
        )


andMap : BitReader a -> BitReader (a -> b) -> BitReader b
andMap a f =
    map2 (<|) f a


map2 :
    (a -> b -> output)
    -> BitReader a
    -> BitReader b
    -> BitReader output
map2 f (BitReader fa) (BitReader fb) =
    BitReader <|
        \state ->
            case fa state of
                Err e ->
                    Err e

                Ok ( a, newState ) ->
                    case fb newState of
                        Err e ->
                            Err e

                        Ok ( b, newerState ) ->
                            Ok ( f a b, newerState )


map3 :
    (a -> b -> c -> output)
    -> BitReader a
    -> BitReader b
    -> BitReader c
    -> BitReader output
map3 f a b c =
    succeed f
        |> andMap a
        |> andMap b
        |> andMap c


map4 :
    (a -> b -> c -> d -> output)
    -> BitReader a
    -> BitReader b
    -> BitReader c
    -> BitReader d
    -> BitReader output
map4 f a b c d =
    succeed f
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d


andThen : (a -> BitReader b) -> BitReader a -> BitReader b
andThen f (BitReader g) =
    BitReader <|
        \s ->
            case g s of
                Ok ( value, newState ) ->
                    let
                        (BitReader h) =
                            f value
                    in
                    h newState

                Err e ->
                    Err e


readBits : Int -> Int -> BitReader Int
readBits numberOfBits base =
    -- TODO be faster for multiples of 8, especially when bitposition = 0
    BitReader <|
        \state ->
            if numberOfBits == 0 then
                Ok ( base, state )

            else
                case fillWindow state of
                    Err e ->
                        Err e

                    Ok d ->
                        let
                            ( val, newTag ) =
                                shiftOffTag d.tag numberOfBits
                        in
                        Ok ( val + base, { tag = newTag, bitsAvailable = d.bitsAvailable - numberOfBits, buffer = d.buffer } )


getBit : BitReader Int
getBit =
    BitReader <|
        \state ->
            if state.bitsAvailable == 0 then
                -- must read new byte
                case runDecoder 1 Decode.unsignedInt8 state of
                    Ok ( tag, newBuffer ) ->
                        let
                            bit =
                                Bitwise.and tag 1

                            newTag =
                                Bitwise.shiftRightZfBy 1 tag
                        in
                        Ok ( bit, { tag = newTag, bitsAvailable = 7, buffer = newBuffer } )

                    Err e ->
                        Err ("getBit > " ++ e)

            else
                let
                    bit =
                        Bitwise.and state.tag 1

                    newTag =
                        Bitwise.shiftRightZfBy 1 state.tag
                in
                Ok ( bit, { buffer = state.buffer, tag = newTag, bitsAvailable = state.bitsAvailable - 1 } )


shiftOntoTag tag value bitsAvailable =
    Bitwise.or tag (Bitwise.shiftLeftBy bitsAvailable value)


shiftOffTag tag numberOfBits =
    let
        val =
            Bitwise.and tag (Bitwise.shiftRightZfBy (16 - numberOfBits) 0xFFFF)

        newTag =
            Bitwise.shiftRightZfBy numberOfBits tag
    in
    ( val, newTag )


fillWindow : State -> Result String State
fillWindow state =
    let
        unsignedInt24 endianness =
            Decode.map2 (\b1 b2 -> shiftOntoTag b1 b2 16)
                (Decode.unsignedInt16 endianness)
                Decode.unsignedInt8

        helper width decoder =
            case runDecoder width decoder state of
                Err e ->
                    Ok state

                Ok ( byte, newBuffer ) ->
                    Ok
                        { tag = shiftOntoTag state.tag byte state.bitsAvailable
                        , bitsAvailable = state.bitsAvailable + (width * 8)
                        , buffer = newBuffer
                        }
    in
    if state.bitsAvailable == 0 then
        helper 4 (Decode.unsignedInt32 LE)

    else if state.bitsAvailable <= 8 then
        helper 3 (unsignedInt24 LE)

    else if state.bitsAvailable <= 16 then
        helper 2 (Decode.unsignedInt16 LE)

    else if state.bitsAvailable < 24 then
        helper 1 Decode.unsignedInt8

    else
        Ok state


runDecoder : Int -> Decode.Decoder a -> State -> Result String ( a, Bytes )
runDecoder width valueDecoder state =
    let
        decoder =
            Decode.map2 Tuple.pair valueDecoder (Decode.bytes (Bytes.width state.buffer - width))
    in
    case Decode.decode decoder state.buffer of
        Just ( byte, newBuffer ) ->
            Ok ( byte, newBuffer )

        Nothing ->
            Err "BitReader.runDecoder: Unexpected end of Bytes"


loop : state -> (state -> BitReader (Step state a)) -> BitReader a
loop state callback =
    BitReader (loopHelp state callback)


loopHelp : state -> (state -> BitReader (Step state a)) -> State -> Result String ( a, State )
loopHelp accum callback state =
    let
        (BitReader decoder) =
            callback accum

        step =
            decoder state
    in
    case step of
        Err e ->
            Err e

        Ok ( Loop newAccum, newState ) ->
            loopHelp newAccum callback newState

        Ok ( Done result, newState ) ->
            Ok ( result, newState )
