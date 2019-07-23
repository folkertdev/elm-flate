module LengthLimitedHuffmanCodes exposing (calculate, merge, package)

{-| Based on <https://www.ics.uci.edu/~dan/pubs/LenLimHuff.pdf>

but the implementation is not as efficient

-}

import Array exposing (Array)
import List.Extra


type alias Node =
    { symbols : Array Int, weight : Int }


emptyNode : Node
emptyNode =
    { symbols = Array.empty, weight = 0 }


singletonNode : Int -> Int -> Node
singletonNode symbol weight =
    { symbols = Array.repeat 1 symbol, weight = weight }


mergeNodes : Node -> Node -> Node
mergeNodes node1 node2 =
    { symbols = Array.append node1.symbols node2.symbols
    , weight = node1.weight + node2.weight
    }


calculate maxBitWidth frequencies =
    let
        source =
            frequencies
                |> Array.indexedMap Tuple.pair
                |> Array.filter (\( _, f ) -> f > 0)
                |> Array.map (\( symbol, weight ) -> singletonNode symbol weight)
                |> Array.toList
                |> List.Extra.stableSortWith (\a b -> compare a.weight b.weight)
                |> Array.fromList

        weighted =
            -- -2 because rust ranges are exclusive, elm's are inclusive
            List.range 0 (maxBitWidth - 2)
                |> List.foldl (\_ w -> merge (package w) source) source

        allSymbols =
            weighted
                |> package
                |> Array.toList
                |> List.concatMap (.symbols >> Array.toList)

        loop symbols accum =
            case symbols of
                [] ->
                    accum

                symbol :: rest ->
                    loop rest (update symbol (\v -> v + 1) accum)
    in
    loop allSymbols (Array.repeat (Array.length frequencies) 0)


update index tagger array =
    case Array.get index array of
        Nothing ->
            array

        Just value ->
            Array.set index (tagger value) array


package : Array Node -> Array Node
package nodes =
    if Array.length nodes >= 2 then
        let
            newLen =
                Array.length nodes // 2

            loop currentNodes accum =
                case currentNodes of
                    self :: other :: rest ->
                        loop rest (mergeNodes self other :: accum)

                    _ ->
                        -- we round down, so 0 and 1 elements are treated the same
                        Array.fromList (List.reverse accum)
        in
        loop (Array.toList nodes) []

    else
        nodes


merge : Array Node -> Array Node -> Array Node
merge x y =
    mergeLoop (Array.toList x) (Array.toList y) Array.empty


mergeLoop : List Node -> List Node -> Array Node -> Array Node
mergeLoop xarr yarr accum =
    case ( xarr, yarr ) of
        ( [], _ ) ->
            Array.append accum (Array.fromList yarr)

        ( _, [] ) ->
            Array.append accum (Array.fromList xarr)

        ( x :: xrest, y :: yrest ) ->
            if x.weight < y.weight then
                mergeLoop xrest yarr (Array.push x accum)

            else
                mergeLoop xarr yrest (Array.push y accum)
