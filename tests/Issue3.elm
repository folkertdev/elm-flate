module Issue3 exposing (suite)

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Flate
import Issue2Data
import LZ77
import Test exposing (..)


deflateInflate opts =
    Encode.string
        >> Encode.encode
        >> Flate.deflateWithOptions opts
        >> Flate.inflate


options =
    [ ( "static, no compression", Flate.Static Flate.NoCompression )
    , ( "static, compression", Flate.Static (Flate.WithWindowSize LZ77.maxWindowSize) )
    , ( "dynamic, no compression", Flate.Dynamic Flate.NoCompression )
    , ( "dynamic, compression", Flate.Dynamic (Flate.WithWindowSize LZ77.maxWindowSize) )
    ]


deflateInflateTest opts str =
    deflateInflate opts str
        |> Maybe.map Bytes.width
        |> Expect.equal (Just (String.length str))


optionTest ( name, opts ) =
    describe name
        [ test "just under 1MB" <|
            \_ ->
                String.repeat (1024 * 1024 - 1) "a"
                    |> deflateInflateTest opts
        , test "exactly 1MB" <|
            \_ ->
                String.repeat (1024 * 1024) "a"
                    |> deflateInflateTest opts
        , test "sligtly more than 1MB" <|
            \_ ->
                String.repeat (1024 * 1024 + 1) "a"
                    |> deflateInflateTest opts
        ]


suite =
    describe "issue 3" <|
        List.map optionTest options
