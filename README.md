# elm-flate

An implementation of [DEFLATE](https://www.ietf.org/rfc/rfc1951.txt) compression.

The deflate format is used in common file formats like gzip, png, and woff.
This package implements vanilla deflate/inflate, as well as gzip and zlib variants.
It additionally exposes LZ77 compression and the Adler32 and Crc32 checksum algorithms.


## Example

```elm
import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Bytes.Decode as Decode

text : String
text =
    "Dyn flam dôve nea"

decodeAsString : Bytes -> Maybe String
decodeAsString buffer =
    let 
        decoder = Decode.string (Bytes.width buffer)
    in 
        Decode.decode decoder buffer

inflate (deflate (Encode.encode (Encode.string text)))
    |> Maybe.andThen decodeAsString
    --> Just "Dyn flam dôve nea"
```

## Performance

Performance is not great for the moment, in particular using LZ77 compression is very slow for large data. 
If all you need is data that looks like deflated data, then raw encoding is the fastest

```elm
deflateWithOptions Flate.Raw
```

If you actually want some compression but are worried about speed, then you can use the `NoCompression` argument. This skips LZ77 compression, but still uses a huffman table to make the data a little smaller at relatively low cost. 

```elm
deflateWithOptions (Flate.Static Flate.NoCompression)
```

Of course, if you really want compression and speed is less of a factor, the default `deflate` will use a dynamic (custom to your data) huffman table and LZ77 compression.

```elm
deflateWithOptions (Flate.Dynamic (Flate.WithWindowSize LZ77.maxWindowSize))
```

With this package as a testing ground, hopefully we can make better primitives that will make this package faster in the future. 
For now it at least works, and we can build thinks on top of it. For instance PNG encoding is now possible.

## Acknowledgements

* The decoder is based on the [libflate](https://docs.rs/libflate/0.1.25/libflate/) rust crate by Takeru Ohta
* The encoder is based on [tiny-inflate](https://github.com/foliojs/tiny-inflate) written by Devon Govett
* Which is a JS port of the  [tinf](https://bitbucket.org/jibsen/tinf/src/default/) c library by Jørgen Ibsen

