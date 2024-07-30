module NestedTuple exposing
    ( cons
    , define
    , empty
    , endFold
    , endMap
    , endMap2
    , endMap3
    , fold
    , map
    , map2
    , map3
    , singleton
    )


do doer doThis doPrev =
    \doRest -> doPrev (doer doThis doRest)


end ender prev =
    prev ender


{-| Define a "fold" for nested tuples of a particular type

    tup =
    ( 1, ( "hello", () ) )

    myFold =
    defineFold
    |> fold (\\n acc -> n + acc)
    |> fold (\\s acc -> String.length s + acc)
    |> endFold

    myFold 0 tup

    --> 6

-}
fold =
    let
        folder foldThis_ foldRest_ acc ( this, rest ) =
            foldRest_ (foldThis_ this acc) rest
    in
    do folder


endFold =
    end (\acc () -> acc)


map =
    do Tuple.mapBoth


endMap =
    end (\() -> ())


define =
    identity


map2 =
    let
        mapper2 mapThis mapRest ( thisA, restA ) ( thisB, restB ) =
            ( mapThis thisA thisB
            , mapRest restA restB
            )
    in
    do mapper2


endMap2 =
    end (\() () -> ())


map3 =
    let
        mapper2 mapThis mapRest ( thisA, restA ) ( thisB, restB ) ( thisC, restC ) =
            ( mapThis thisA thisB thisC
            , mapRest restA restB restC
            )
    in
    do mapper2


endMap3 =
    end (\() () () -> ())


empty : ()
empty =
    ()


singleton : a -> ( a, () )
singleton a =
    cons a empty


cons : a -> b -> ( a, b )
cons a tup =
    ( a, tup )
