module Examples exposing (..)

import NestedTuple exposing (..)


myTuple : ( Int, ( String, () ) )
myTuple =
    cons 2 (singleton "hello")


myMapper : ( Int, ( String, () ) ) -> ( Int, ( String, () ) )
myMapper =
    define
        |> map (\n -> n * 2)
        |> map (\s -> s ++ s)
        |> endMap


mapped : ( Int, ( String, () ) )
mapped =
    myMapper myTuple


myFolder : Int -> ( Int, ( String, () ) ) -> Int
myFolder =
    define
        |> fold (\n acc -> n + acc)
        |> fold (\s acc -> String.length s + acc)
        |> endFold


folded : Int
folded =
    myFolder 0 myTuple


myMapper2 : ( Int, ( String, () ) ) -> ( Int, ( String, () ) ) -> ( Int, ( Int, () ) )
myMapper2 =
    define
        |> map2 (\a b -> a + b)
        |> map2 (\a b -> String.length (a ++ b))
        |> endMap2


mapped2 : ( Int, ( Int, () ) )
mapped2 =
    myMapper2 myTuple myTuple


myMapper3 : ( Int, ( String, () ) ) -> ( Int, ( String, () ) ) -> ( Int, ( String, () ) ) -> ( Int, ( Int, () ) )
myMapper3 =
    define
        |> map3 (\a b c -> a + b + c)
        |> map3 (\a b c -> String.length (a ++ b ++ c))
        |> endMap3


mapped3 : ( Int, ( Int, () ) )
mapped3 =
    myMapper3 myTuple myTuple myTuple
