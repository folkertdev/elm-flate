module PrefixTable exposing (Prefix, PrefixTable, createPrefix, insert, length, new)

import Array exposing (Array)
import Bitwise
import Dict exposing (Dict)


{-| Maximum backward distance of a pointer.
-}
max_distance =
    32768


{-| Maximum size of a sliding window.
-}
max_window_size =
    max_distance


type PrefixTable
    = Small (Dict Int Int)
    | Large LargePrefixTable


type Prefix
    = Prefix Int


createPrefix : Int -> Int -> Int -> Prefix
createPrefix a b c =
    Bitwise.shiftLeftBy 16 a
        |> Bitwise.or (Bitwise.or (Bitwise.shiftLeftBy 8 b) c)
        |> Prefix


length : PrefixTable -> Int
length table =
    case table of
        Small dict ->
            Dict.size dict

        Large _ ->
            -1


new : Int -> PrefixTable
new nbytes =
    -- if nbytes < max_window_size then
    if True then
        Small Dict.empty

    else
        Large newLargePrefixTable


insert : Prefix -> Int -> PrefixTable -> ( PrefixTable, Maybe Int )
insert (Prefix prefix) position ptable =
    case ptable of
        Small dict ->
            case Dict.get prefix dict of
                Nothing ->
                    ( Small (Dict.insert prefix position dict), Nothing )

                Just oldValue ->
                    ( Small (Dict.insert prefix position dict), Just oldValue )

        Large (LargePrefixTable array) ->
            let
                -- ( p0, p1, p2 ) = prefix
                i =
                    -- Bitwise.shiftLeftBy 8 p0 + p1
                    Bitwise.shiftRightBy 8 prefix

                p2 =
                    Bitwise.and 0xFF prefix
            in
            case Array.get i array of
                Nothing ->
                    ( ptable, Nothing )

                Just positions ->
                    let
                        size =
                            Array.length positions

                        go k accum =
                            if k < size then
                                case Array.get k accum of
                                    Nothing ->
                                        Err ()

                                    Just ( key, oldValue ) ->
                                        if key == p2 then
                                            Ok ( Large (LargePrefixTable (Array.set i (Array.set k ( key, position ) positions) array)), Just oldValue )

                                        else
                                            go (k + 1) accum

                            else
                                Err ()
                    in
                    case go 0 positions of
                        Ok value ->
                            value

                        Err _ ->
                            ( Large (LargePrefixTable (Array.set i (Array.push ( p2, position ) positions) array)), Nothing )


type LargePrefixTable
    = LargePrefixTable (Array (Array ( Int, Int )))


newLargePrefixTable =
    LargePrefixTable (Array.repeat 0xFFFF Array.empty)



-- LargePrefixTable Dict.empty
