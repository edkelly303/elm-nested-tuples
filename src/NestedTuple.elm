module NestedTuple exposing
    ( accessors
    , cons
    , define
    , defineAccessors
    , empty
    , endAccessors
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


{-| Define a "folder" for nested tuples of a particular type

    tuple =
        ( 1, ( "hello", () ) )

    myFolder =
        define
            |> fold (\\n acc -> n + acc)
            |> fold (\\s acc -> String.length s + acc)
            |> endFold

    myFolder 0 tuple

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


defineAccessors =
    { get = identity
    , set = identity
    , getters = identity
    , setters = identity
    }


accessors prev =
    { get = prev.get << tail
    , set = prev.set << Tuple.mapSecond
    , getters = prev.getters << cons (prev.get << head)
    , setters = prev.setters << cons (prev.set << (\val tuple -> cons val (tail tuple)))
    }


endAccessors prev =
    { getters = prev.getters ()
    , setters = prev.setters ()
    }
