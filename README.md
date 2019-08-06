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

## How does DEFLATE work? 

The deflate algorithm is a combination of two lossless compression techniques: run-length encoding and huffman codes.

**Run-length encoding** tries to find and abstract common sequences in the data. For instance `"aaaaa"` can be represented as `"a"<distance=1, length=4>`: the character `"a"`, and then an instruction to go 1 position back, and read 4 characters from that position. Deflate uses the LZ77 algorithm to find the repeated sequences.

**Huffman codes** translate the bytes into a language with shorter words. We pick an alphabet, in this case with the characters `0` and `1`, and generate all the words over that alphabet (e.g. `10`, `110`, `1111`) up to a certain length. Then we map bytes of our data to "words" in our language. The trick is to pick the shortest words for the most common bytes.

This is a high-level overview: Huffman codes are more complicated and in deflate they work together with the run-length encoding to be more compact. In any case I hope this gives some background. 

## Performance

The performance of this package is tricky. 

So you have to think about what is most important to you

**The data needs to look like it is deflated, but I don't need compression**

In this case raw encoding is the fastest. It performs no compression but produces valid DEFLATE data.

```elm
deflateWithOptions Flate.Raw
```

**I want some compression, but speed is important**

You can use the `NoCompression` argument. It skips LZ77 compression, but still uses a huffman code to make the data a little smaller at relatively low cost. 

```elm
deflateWithOptions (Flate.Static Flate.NoCompression)
```
**I want compression, speed is less important**

The default `deflate` will use a dynamic (custom to your data) huffman table and LZ77 compression.

```elm
deflateWithOptions (Flate.Dynamic (Flate.WithWindowSize LZ77.maxWindowSize))
```

## Acknowledgements

* The decoder is based on the [libflate](https://docs.rs/libflate/0.1.25/libflate/) rust crate by Takeru Ohta
* The encoder is based on [tiny-inflate](https://github.com/foliojs/tiny-inflate) written by Devon Govett
* Which is a JS port of the  [tinf](https://bitbucket.org/jibsen/tinf/src/default/) c library by Jørgen Ibsen

