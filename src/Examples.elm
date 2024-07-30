module Examples exposing (..)

import NestedTuple exposing (..)


myTuple =
    cons 2 (singleton "hello")


myMapper =
    define
        |> map (\n -> n * 2)
        |> map (\s -> s ++ s)
        |> endMap


mapped =
    myMapper myTuple


myFolder =
    define
        |> fold (\n acc -> n + acc)
        |> fold (\s acc -> String.length s + acc)
        |> endFold


folded =
    myFolder 0 myTuple


myMapper2 =
    define
        |> map2 (\a b -> a + b)
        |> map2 (\a b -> String.length (a ++ b))
        |> endMap2


mapped2 =
    myMapper2 myTuple myTuple


myMapper3 =
    define
        |> map3 (\a b c -> a + b + c)
        |> map3 (\a b c -> String.length (a ++ b ++ c))
        |> endMap3


mapped3 =
    myMapper3 myTuple myTuple myTuple


getSet =
    defineAccessors
        |> accessors
        |> accessors
        |> endAccessors


myHead =
    let
        firstGetter =
            head getSet.getters
    in
    firstGetter myTuple


setTuple =
    let
        firstSetter =
            head getSet.setters

        secondSetter =
            head (tail getSet.setters)
    in
    myTuple
        |> firstSetter 42
        |> secondSetter "cheese"
