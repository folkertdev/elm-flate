# elm-flate

An implementation of [DEFLATE](https://www.ietf.org/rfc/rfc1951.txt) compression.

The deflate format is used in common file formats like gzip, png, and woff.
This package implements vanilla deflate/inflate, as well as gzip and zlib variants.
It additionally exposes LZ77 compression and the Adler32 and Crc32 checksum algorithms.


## Example

```elm
import Bytes
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

LZ77 is a method of finding repeating subsections of the data, and inserting backward references whenever repeated subsections are found. 
This algorithm does a lot of subsection matching, and consequently a lot of array lookups. 
This is always the most computationally expensive step in deflate encoding, but the particulars of elm's `Bytes` and `Array` implementations make it worse in elm at the moment. 

But with this package as a testing ground, hopefully we can make better primitives that will make this package faster in the future. 
For now it at least works, and we can build thinks on top of it. For instance PNG encoding is now possible.
