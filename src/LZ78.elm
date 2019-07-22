module LZ78 exposing (decode, encode)

import Array exposing (Array)
import Dict


type alias Token a =
    ( Int, a )


token x y =
    ( x, y )



-- | An LZ78 compressed 'Generator'.


type LZ78 a
    = Cons (Token a) (LZ78 a)
    | Nil



-- | /O(n log n)/ Contruct an LZ78-compressed 'Generator' using a 'Dict' internally.


encode : List comparable -> LZ78 comparable
encode =
    let
        go d f p list =
            let
                _ =
                    Debug.log "dict" d
            in
            case list of
                [] ->
                    Nil

                [ c ] ->
                    Cons (token p c) Nil

                c :: cs ->
                    let
                        t =
                            token p c
                    in
                    case Dict.get t d of
                        Just p_ ->
                            go d f p_ cs

                        Nothing ->
                            Cons t (go (Dict.insert t f d) (f + 1) 0 cs)
    in
    go Dict.empty 1 0


mapTo : (b -> a) -> List a -> LZ78 b -> List a
mapTo =
    let
        go : Array (List a) -> (b -> a) -> List a -> LZ78 b -> List a
        go cache f m structure =
            let
                _ =
                    Debug.log "cache" cache
            in
            case structure of
                Nil ->
                    m

                Cons ( width, c ) ws ->
                    let
                        v =
                            case Array.get width cache of
                                Just value ->
                                    value ++ [ f c ]

                                Nothing ->
                                    -- unreachable!
                                    [ f c ]
                    in
                    m ++ go (Array.push v cache) f v ws
    in
    go (Array.repeat 1 [])


decode =
    reduce << Debug.log "encoded"


reduce : LZ78 a -> List a
reduce =
    mapReduce identity


mapReduce : (b -> a) -> LZ78 b -> List a
mapReduce f =
    mapTo f []
