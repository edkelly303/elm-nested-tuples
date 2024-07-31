module NestedTuple exposing
    ( appender
    , cons
    , define
    , defineGetters
    , defineSetters
    , empty
    , endAppender
    , endFolder
    , endFolder2
    , endGetters
    , endMapper
    , endMapper2
    , endMapper3
    , endSetters
    , folder
    , folder2
    , getter
    , head
    , mapHead
    , mapTail
    , mapper
    , mapper2
    , mapper3
    , setter
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



-- Utility functions


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



-- Defining mappers, folders etc.


define =
    identity



-- Appenders


appender value prev next =
    prev ( value, next )


endAppender prev =
    prev empty



-- Folders


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
folder =
    let
        folder_ foldHead foldTail acc tuple =
            let
                acc2 =
                    foldHead (head tuple) acc
            in
            foldTail acc2 (tail tuple)
    in
    do folder_


endFolder =
    end (\acc _ -> acc)


folder2 =
    let
        folder2_ foldHead foldTail acc tuple1 tuple2 =
            let
                acc2 =
                    foldHead (head tuple1) (head tuple2) acc
            in
            foldTail acc2 (tail tuple1) (tail tuple2)
    in
    do folder2_


endFolder2 =
    end (\acc _ _ -> acc)



-- Mappers


mapper =
    let
        mapper_ fHead fTail a =
            a
                |> mapHead fHead
                |> mapTail fTail
    in
    do mapper_


endMapper =
    end (\_ -> empty)


mapper2 =
    let
        mapper2_ fHead fTail a b =
            cons
                (fHead (head a) (head b))
                (fTail (tail a) (tail b))
    in
    do mapper2_


endMapper2 =
    end (\_ _ -> empty)


mapper3 =
    let
        mapper3_ fHead fTail a b c =
            cons
                (fHead (head a) (head b) (head c))
                (fTail (tail a) (tail b) (tail c))
    in
    do mapper3_


endMapper3 =
    end (\_ _ _ -> empty)



-- Getters and setters


defineGetters =
    { get = identity
    , getters = identity
    }


getter prev =
    { get = prev.get << tail
    , getters = prev.getters << cons (prev.get << head)
    }


endGetters prev =
    prev.getters empty


defineSetters =
    { set = identity
    , setters = identity
    }


setter prev =
    { set = prev.set << mapTail
    , setters = prev.setters << cons (prev.set << mapHead << always)
    }


endSetters prev =
    prev.setters empty



-- Magic


do doer doThis doPrev =
    \doRest -> doPrev (doer doThis doRest)


end ender prev =
    prev ender
