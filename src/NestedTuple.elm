module NestedTuple exposing
    ( cons
    , define
    , empty
    , endFold
    , endMap
    , endMap2
    , endMap3
    , fold
    , head
    , map
    , map2
    , map3
    , singleton
    , tail
    )


do doer doThis doPrev =
    \doRest -> doPrev (doer doThis doRest)


end ender prev =
    prev ender


{-| Define a "fold" for nested tupleles of a particular type

    tuple =
    ( 1, ( "hello", () ) )

    myFold =
    defineFold
    |> fold (\\n acc -> n + acc)
    |> fold (\\s acc -> String.length s + acc)
    |> endFold

    myFold 0 tuple

    --> 6

-}
fold =
    let
        flip f arg1 arg2 =
            f arg2 arg1

        folder foldHead foldTail acc tuple =
            acc
                |> foldHead (head tuple)
                |> flip foldTail (tail tuple)
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
        mapper2 mapHead mapTail a b =
            cons
                (mapHead (head a) (head b))
                (mapTail (tail a) (tail b))
    in
    do mapper2


endMap2 =
    end (\() () -> ())


map3 =
    let
        mapper2 mapHead mapTail a b c =
            cons
                (mapHead (head a) (head b) (head c))
                (mapTail (tail a) (tail b) (tail c))
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
cons a tuple =
    ( a, tuple )


head =
    Tuple.first


tail =
    Tuple.second
