module Examples exposing (..)

import NestedTuple exposing (..)


myTuple =
    cons 2.5 (cons 2 (singleton "hello"))


myMapper =
    define
        |> mapper (\n -> n / 2.0)
        |> mapper (\n -> n * 2)
        |> mapper (\s -> s ++ s)
        |> endMapper


mapped =
    myMapper myTuple


myFolder =
    define
        |> folder (\n acc -> round n + acc)
        |> folder (\n acc -> n + acc)
        |> folder (\s acc -> String.length s + acc)
        |> endFolder


folded =
    myFolder 0 myTuple


myFolder2 =
    define
        |> folder2 (\n1 n2 acc -> round (n1 + n2) + acc)
        |> folder2 (\n1 n2 acc -> n1 + n2 + acc)
        |> folder2 (\s1 s2 acc -> String.length (s1 ++ s2) + acc)
        |> endFolder2


folded2 =
    myFolder2 0 myTuple myTuple


myMapper2 =
    define
        |> mapper2 (\a b -> a / b)
        |> mapper2 (\a b -> a + b)
        |> mapper2 (\a b -> String.length (a ++ b))
        |> endMapper2


mapped2 =
    myMapper2 myTuple myTuple


myMapper3 =
    define
        |> mapper3 (\a b c -> a / b / c)
        |> mapper3 (\a b c -> a + b + c)
        |> mapper3 (\a b c -> String.length (a ++ b ++ c))
        |> endMapper3


mapped3 =
    myMapper3 myTuple myTuple myTuple


getters =
    defineGetters
        |> getter
        |> getter
        |> endGetters


got =
    let
        getFirst =
            head getters

        getSecond =
            head (tail getters)
    in
    { first = getFirst myTuple
    , second = getSecond myTuple
    }
