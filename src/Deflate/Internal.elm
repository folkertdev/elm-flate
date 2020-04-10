module Deflate.Internal exposing (compress, encodeDynamic, encodeRaw, encodeStatic)

import Array exposing (Array)
import Bitwise
import ByteArray
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode exposing (Encoder)
import Deflate.BitWriter as BitWriter exposing (BitWriter)
import Deflate.Symbol as Symbol
import Huffman
import LZ77


max_non_compressed_block_size : Int
max_non_compressed_block_size =
    0xFFFF


default_block_size : Int
default_block_size =
    1024 * 1024



-- Chunks


{-| Split `Bytes` into blocks
-}
chunks : Int -> Bytes -> List ( Bool, Bytes )
chunks chunkSize buffer =
    case Decode.decode (Decode.loop ( Bytes.width buffer, [] ) (chunksHelp chunkSize)) buffer of
        Nothing ->
            [ ( True, Encode.encode (Encode.sequence []) ) ]

        Just [] ->
            [ ( True, Encode.encode (Encode.sequence []) ) ]

        Just value ->
            value


chunksHelp chunkSize ( sizeRemaining, accum ) =
    if sizeRemaining == 0 then
        -- only happens when the input buffer is empty
        Decode.succeed (Decode.Done [])

    else if chunkSize >= sizeRemaining then
        Decode.bytes sizeRemaining |> Decode.map (\new -> Done (List.reverse (( True, new ) :: accum)))

    else
        Decode.bytes chunkSize |> Decode.map (\new -> Loop ( sizeRemaining - chunkSize, ( False, new ) :: accum ))



-- Raw


encodeRaw : Bytes -> Bytes
encodeRaw buffer =
    chunks (min max_non_compressed_block_size default_block_size) buffer
        |> List.foldl (\chunk first -> first |> encodeRawBlock chunk) BitWriter.empty
        |> BitWriter.run
        |> Encode.sequence
        |> Encode.encode


encodeRawBlock : ( Bool, Bytes ) -> BitWriter -> BitWriter
encodeRawBlock ( isLastBlock, buffer ) bitWriter =
    let
        byteArray =
            ByteArray.fromBytes buffer

        size =
            min (Array.length byteArray) max_non_compressed_block_size

        sliced =
            Array.slice 0 size byteArray
    in
    bitWriter
        |> BitWriter.writeBit isLastBlock
        |> BitWriter.writeBits 2 0
        |> BitWriter.flush
        |> BitWriter.writeEncoder (Encode.unsignedInt16 LE size)
        |> BitWriter.writeEncoder (Encode.unsignedInt16 LE (Bitwise.complement size))
        |> BitWriter.writeEncoder (Encode.bytes (ByteArray.toBytes sliced))



-- Dynamic


encodeDynamic : Maybe Int -> Bytes -> Bytes
encodeDynamic windowSize buffer =
    let
        encodedChunks =
            List.map (encodeDynamicBlock windowSize) (chunks default_block_size buffer)
    in
    List.foldl (\chunk first -> first |> chunk) BitWriter.empty encodedChunks
        |> BitWriter.flush
        |> BitWriter.run
        |> Encode.sequence
        |> Encode.encode


encodeDynamicBlock : Maybe Int -> ( Bool, Bytes ) -> BitWriter -> BitWriter
encodeDynamicBlock windowSize ( isLastBlock, buffer ) bitWriter =
    bitWriter
        |> BitWriter.writeBit isLastBlock
        |> BitWriter.writeBits 2 2
        |> encodeCompressDynamic windowSize buffer


encodeCompressDynamic : Maybe Int -> Bytes -> BitWriter -> BitWriter
encodeCompressDynamic maybeWindowSize buf bitWriter =
    let
        compressed =
            compress maybeWindowSize buf

        huffmanTree =
            Symbol.buildDynamicHuffmanCodec compressed

        huffmanTreeWriter : BitWriter
        huffmanTreeWriter =
            bitWriter
                |> Symbol.writeDynamicHuffmanCodec huffmanTree
    in
    Array.foldl (\symbol first -> first |> Symbol.encode symbol huffmanTree) huffmanTreeWriter compressed



-- Static


encodeStatic : Maybe Int -> Bytes -> Bytes
encodeStatic windowSize buffer =
    chunks default_block_size buffer
        |> List.foldl (\chunk first -> first |> encodeStaticBlock windowSize chunk) BitWriter.empty
        |> BitWriter.flush
        |> BitWriter.run
        |> Encode.sequence
        |> Encode.encode


encodeStaticBlock : Maybe Int -> ( Bool, Bytes ) -> BitWriter -> BitWriter
encodeStaticBlock windowSize ( isLastBlock, buffer ) bitWriter =
    bitWriter
        |> BitWriter.writeBit isLastBlock
        |> BitWriter.writeBits 2 1
        |> encodeCompressStatic windowSize buffer


encodeCompressStatic : Maybe Int -> Bytes -> BitWriter -> BitWriter
encodeCompressStatic maybeWindowSize buf bitWriter =
    let
        compressed =
            compress maybeWindowSize buf

        -- the fixed huffman tree does not need to be encoded
        huffmanTrees =
            Huffman.hardcodedStaticHuffmanTree
    in
    Array.foldl (\symbol first -> first |> Symbol.encode symbol huffmanTrees) bitWriter compressed



-- Helpers


compress : Maybe Int -> Bytes -> Array Symbol.Symbol
compress maybeWindowSize buf =
    case maybeWindowSize of
        Nothing ->
            buf
                |> ByteArray.fromBytes
                |> Array.map Symbol.Literal
                |> Array.push Symbol.EndOfBlock

        Just windowSize ->
            buf
                |> LZ77.encodeWithOptions { windowSize = windowSize }
                |> Array.map codeToSymbol
                |> Array.push Symbol.EndOfBlock


codeToSymbol code =
    case code of
        LZ77.Literal v ->
            Symbol.Literal v

        LZ77.Pointer length backwardDistance ->
            Symbol.Share length backwardDistance
