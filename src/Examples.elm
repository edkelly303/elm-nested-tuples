module Examples exposing (..)

import NestedTuple exposing (..)


myTuple : ( Int, ( String, () ) )
myTuple =
    cons 2 (singleton "hello")


myMap : ( Int, ( String, () ) ) -> ( Int, ( String, () ) )
myMap =
    define
        |> map (\n -> n * 2)
        |> map (\s -> s ++ s)
        |> endMap


mapped : ( Int, ( String, () ) )
mapped =
    myMap myTuple


myFold : Int -> ( Int, ( String, () ) ) -> Int
myFold =
    define
        |> fold (\n acc -> n + acc)
        |> fold (\s acc -> String.length s + acc)
        |> endFold


folded : Int
folded =
    myFold 0 myTuple


myMap2 : ( Int, ( String, () ) ) -> ( Int, ( String, () ) ) -> ( Int, ( Int, () ) )
myMap2 =
    define
        |> map2 (\a b -> a + b)
        |> map2 (\a b -> String.length (a ++ b))
        |> endMap2


mapped2 : ( Int, ( Int, () ) )
mapped2 =
    myMap2 myTuple myTuple


myMap3 : ( Int, ( String, () ) ) -> ( Int, ( String, () ) ) -> ( Int, ( String, () ) ) -> ( Int, ( Int, () ) )
myMap3 =
    define
        |> map3 (\a b c -> a + b + c)
        |> map3 (\a b c -> String.length (a ++ b ++ c))
        |> endMap3


mapped3 : ( Int, ( Int, () ) )
mapped3 =
    myMap3 myTuple myTuple myTuple
