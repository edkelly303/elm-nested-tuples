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
    , mapBoth
    , mapHead
    , mapTail
    , singleton
    , tail
    )

{- From these first four functions (`empty`, `cons`, `head` and `tail`), all the
   others can be derived. 
   
   So if you wanted to represent a nested tuple as something other than a tuple,
   for example `type alias MyTuple = { head : a, tail : b }` or `type MyTuple a
   b = MyTuple a b`, just vendor this package and redefine these four functions,
   and everything else should _just work_.
-}


empty =
    ()


cons a tuple =
    ( a, tuple )


head =
    Tuple.first


tail =
    Tuple.second


singleton a =
    cons a empty


mapHead f tuple =
    cons
        (f (head tuple))
        (tail tuple)


mapTail f tuple =
    cons
        (head tuple)
        (f (tail tuple))


mapBoth headF tailF tuple =
    cons
        (headF (head tuple))
        (tailF (tail tuple))


define =
    identity


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
    end (\acc _ -> acc)


map =
    do mapBoth


endMap =
    end (\_ -> empty)


map2 =
    let
        mapper2 map2Head map2Tail a b =
            cons
                (map2Head (head a) (head b))
                (map2Tail (tail a) (tail b))
    in
    do mapper2


endMap2 =
    end (\_ _ -> empty)


map3 =
    let
        mapper2 map3Head map3Tail a b c =
            cons
                (map3Head (head a) (head b) (head c))
                (map3Tail (tail a) (tail b) (tail c))
    in
    do mapper2


endMap3 =
    end (\_ _ _ -> empty)


defineAccessors =
    { get = identity
    , set = identity
    , getters = identity
    , setters = identity
    , updaters = identity
    }


accessors prev =
    { get = prev.get << tail
    , set = prev.set << mapTail
    , getters = prev.getters << cons (prev.get << head)
    , setters = prev.setters << cons (prev.set << mapHead << always)
    , updaters = prev.updaters << cons (prev.set << mapHead)
    }


endAccessors prev =
    { getters = prev.getters empty
    , setters = prev.setters empty
    , updaters = prev.updaters empty
    }


do doer doThis doPrev =
    \doRest -> doPrev (doer doThis doRest)


end ender prev =
    prev ender
