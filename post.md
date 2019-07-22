# elm-flate & elm-brotli: (de)compression in elm

Recently I've been working on [elm-brotli](https://package.elm-lang.org/packages/folkertdev/elm-brotli/latest/), a brotli decoder, and [elm-flate](), an implementation of DEFLATE compression.

Brotli is used in woff2 font files, DEFLATE is used in among others the gzip, png and woff file formats. 
Font parsing has some cool uses (static layout, automatic text breaking in svg), generating png images is useful for webgl/games.
These packages are low-level building blocks that can be used in many creative ways. 

Unfortunately, they are quite slow. Performance is not horrible, but it's not good either. Most of the issues arise because the current `elm/bytes` package does not support arrays very well.
The algorithms assume that the data is represented in array form. They use and encode indices into arrays in ways that would be hard (and inefficient) to do with just the current `Bytes.Decode.Decoder` type.
The `elm/bytes` docs ask for use cases where the current api doesn't quite work, so here goes.

## Problem 1: Array Decoding/Encoding

The use of arrays means that the data has to actually be converted from `Bytes` to `Array Int`, then re-encoded again after decompression.
The current `elm/bytes` implementation does not do that very efficiently. For instance, the "natural" way to encode a array of bytes is slower than turning the whole thing into a string, and then encoding the string:

```
 Benchmark.compare "encode array of bytes"
    "string"
    (\_ ->
        data
            |> Array.foldl (\e a -> a ++ String.fromChar (Char.fromCode e)) ""
            |> Encode.string
            |> Encode.encode
    )
    "bytes"
       (\_ ->
           data
               |> Array.foldr (\e a -> Encode.unsignedInt8 e :: a) []
               |> Encode.sequence
               |> Encode.encode
       )
```

with `--optimize`, `"bytes"` is usually about 15% slower than `"string"`. The culprit is `Encode.sequence`, which internally needs to check the width of all elements of its argument. 
That makes sense in general, but is very wasteful when you know the width of all the elements to be the same up-front, like when you have an array bytes. 

I actually use a more optimized encoder that groups bytes into sets of 4, then encodes as `unsignedInt32`. That means only a quarter the number of encoders, therefore a quarter the number of width checks. 
Fewer list elements also helps with garbage collection since all JS numbers use 8 bytes anyway, it's a good idea to use them as efficiently as we can (which is 4 bytes per number because JS numbers).

This problem could be solved without large changes to the package. For instance a `unsignedInt8Array : Array Int -> Encoder` (and corresponding decoder) would help a lot. 


## Problem 2: Large constants

These packages use some rather large constants (brotli uses a dictionary array of over 200K elements), and initializing them is pretty slow.
Currently arrays are initialized as `elmArrayFromList (ListFromJSArray jsArray)` (seems to) allocate the data 3 times. At least the `List Int -> Array Int` could be removed with a rewrite rule in the compiler.

## Problem 3: Array Usage

Many of the algorithms expect fast array access, for instance [run-length encoding](https://en.wikipedia.org/wiki/Run-length_encoding) and corresponding decoding. It can turn a string like `aaaaa` into `a<distance=1, length=4>`. This algorithm needs to find common substrings in the byte array, so we can store it once and then reference it from other locations where it occurs. In an imperative language, that would look like this: 

```python
maxLength = 258
length = 0
while buffer[i] == buffer[j] && length < maxLength:
    length += 1
    i += 1
    j += 1
```

The elm version is currently:

```elm
longestCommonPrefixLoop : Int -> Int -> Int -> Int -> Array Int -> Int
longestCommonPrefixLoop i j maxLength length buffer =
    if i < maxLength then
        case Array.get i buffer of
            Nothing ->
                length

            Just value1 ->
                case Array.get j buffer of
                    Nothing ->
                        length

                    Just value2 ->
                        -- use arithmetic to force inline the compare
                        if (value1 - value2) == 0 then
                            longestCommonPrefixLoop (i + 1) (j + 1) limit (length + 1) buffer

                        else
                            length

    else
        length
```

Because the definition is tail-recursive, it gets compiled to javascript that looks reasonably similar, but there are some important differences: 

* the `buffer` is an elm `Array`. Its performance is generally good, but the byte arrays can become so large that just the sheer number of array operations becomes a problem. Additonally, in contrast to c/rust, every element takes 8 bytes (in JS number use 64 bits) while only 1 byte is really needed for our data. So just loading the 1MB file as an array uses 8MB of memory. 
* I believe we don't benefit (as much) from caching on the cpu because we're using a high-level language. In c and similar languages, accessing adjacent elements is typically faster than random access because the relevant section of the array is cached and does not need to be retrieved from RAM). 
* and the big one: the `Maybe` wrapping. The extra alloctation, case matches, and garbage collection time for every array access are a killer. 

### Solutions

I think a dedicated `ByteArray` type makes sense, but there are some challenges in functional languages because of immutability, and many other details to think about. 

1. **storage**: a `ByteArray` should use only one byte of storage per byte of data. This could be built on top of `UInt8Array`.
2. **conversion**: Conversion between `ByteArray` and `Array Int`/`List Int` should be fast, not only in code but also when large literals are used. 
3. **maybes**: should be avoided. I think this is possible with a lot of design work, focussing on slices and folds instead of individual accesses.
    Folds have the nice property that they don't need to wrap every element in a maybe and they can efficiently go through adjacent elements. 
    A problem is that currently folds can only start at the very start or end of an array. Starting anywhere else requires a `Array.slice` which will allocate.
    So currently repeated `Array.get` is the most efficient.
4. **immutability**: The tricky thing about c style arrays (big chunks of contiguous memory) in functional languages is that whenever an element is updated, we'd have to copy the whole thing to guarantee immutability.
    But we don't want to copy our 1MB byte array whenever we make a change, that would defeat the whole purpose of a dedicated data structure (speed and memory efficiency).

    But so far I haven't really needed random write access into these large arrays, the main operations i've used are:

    * `Array.get` an element somewhere
    * working with a subsection: often a combination of `Array.slice` and `Array.foldl` better expresses what's going on, but because of the cost of `Array.slice`, a loop of repeated `Array.get` is usually faster.
    * `Array.push` onto the end

    So once data is in the array, it (almost always) stays there unchanged until the whole array is encoded into `Bytes` again. 

## Conclusion

I hope that with these packages (and perhaps some real examples that use them) we can create more efficient primitives for working with binary data.  

Also there are probably still improvements that can be made to the code/algorithms with the current primitives (help welcome!).
But especially for `elm-flate`, the profiling is so dominated by datastructure operations (array, list, dict), byte array en/decoding, and GC that it's hard to see what other bottlenecks there are. 


### P.S.

This is purely speculative, but something I couldn't help thinking about as I repeatedly wrote out the record updates for a [35-element record](https://github.com/folkertdev/elm-brotli/blob/master/src/State.elm#L680).
To be immutable, elm data structures regularly need to copy parts of their contents. That is because the mutation might be observed: 

```python
x = Array.fromList [1,2,3]      x = [1,2,3]
y = Array.push 42 x             y = x
                                y.append(42)

l = Array.length x              l = len(x) 
    --> 3                           --> 4
```

In elm, the push cannot update the value in-place, because it would be observable by `x`. But actually we can use mutation if nobody can observe it. In the above example, if we're sure that there exist no other references to the list pointed to by `y`, then we could update it in-place in the elm version. The [ST](https://hackage.haskell.org/package/array-0.5.3.0/docs/Data-Array-ST.html) mechanism in haskell uses this idea. It is not always pretty, but it is safe and efficient. 

Another approach is uniqueness typing. The type system keeps track of uniqueness of references, and the guarantees that in-place mutation can be used safely. For byte arrays that would mean efficient random write access is possible because the array does not need to be copied. 

More generally, it means fast record updates. Record updates are a bottleneck because of how the update is implemented (and it is implemented that way to keep the bundle small). But records - most importantly the `Model` - are unique, which means that with uniqueness typing we could update them efficiently without an increase in budle size. This feature would really change how the language looks and works, but it also seems a good fit for elm because of its programming model (having one piece of state) and its common use of records. 
