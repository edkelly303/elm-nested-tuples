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
    , endFolder3
    , endGetters
    , endMapper
    , endMapper2
    , endMapper3
    , endSetters
    , folder
    , folder2
    , folder3
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



-- Define appenders, mappers, folders etc.


define =
    identity



-- Appender


appender value prev next =
    prev ( value, next )


endAppender prev =
    prev empty



-- Folders


folder =
    let
        folder_ foldHead foldTail accForHead tuple =
            let
                accForTail =
                    foldHead (head tuple) accForHead
            in
            foldTail accForTail (tail tuple)
    in
    do folder_


endFolder =
    end (\acc _ -> acc)


folder2 =
    let
        folder2_ foldHead foldTail accForHead tuple1 tuple2 =
            let
                accForTail =
                    foldHead (head tuple1) (head tuple2) accForHead
            in
            foldTail accForTail (tail tuple1) (tail tuple2)
    in
    do folder2_


endFolder2 =
    end (\acc _ _ -> acc)


folder3 =
    let
        folder3_ foldHead foldTail accForHead tuple1 tuple2 tuple3 =
            let
                accForTail =
                    foldHead (head tuple1) (head tuple2) (head tuple3) accForHead
            in
            foldTail accForTail (tail tuple1) (tail tuple2) (tail tuple3)
    in
    do folder3_


endFolder3 =
    end (\acc _ _ _ -> acc)



-- Mappers


mapper =
    let
        mapper_ fHead fTail a =
            cons
                (fHead (head a))
                (fTail (tail a))
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
