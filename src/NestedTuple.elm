module NestedTuple exposing
    ( empty, cons, head, tail
    , singleton, mapHead, mapTail
    , define
    , appender, endAppender
    , mapper, endMapper, mapper2, endMapper2, mapper3, endMapper3
    , folder, endFolder, folder2, endFolder2, folder3, endFolder3
    , defineGetters, getter, endGetters
    , defineSetters, setter, endSetters
    )

{-| This library provides utility functions for working with nested tuples of
the form `( a, ( b, ( c, () ) ) )`.


# Primitives

@docs empty, cons, head, tail


### Note

From the four functions `empty`, `cons`, `head` and `tail`, all the
others can be derived.

So if you wanted to represent a nested tuple as something other than a tuple,
for example `type alias MyTuple = { head : a, tail : b }` or `type MyTuple a
b = MyTuple a b`, you could just vendor this package and redefine these four
functions for your data type, and everything else should _just work_.


# Utilities

@docs singleton, mapHead, mapTail


# Appenders, mappers and folders

@docs define


## Appenders

@docs appender, endAppender


## Mappers

@docs mapper, endMapper, mapper2, endMapper2, mapper3, endMapper3


## Folders

@docs folder, endFolder, folder2, endFolder2, folder3, endFolder3


## Getters

@docs defineGetters, getter, endGetters


## Setters

@docs defineSetters, setter, endSetters

-}


{-| Create an empty nested tuple.
-}
empty =
    ()


{-| Prepend a value to a nested tuple.
-}
cons a tuple =
    ( a, tuple )


{-| Get the head (first element) of a nested tuple.
-}
head =
    Tuple.first


{-| Get the tail (everything except the first element) of a nested tuple.
-}
tail =
    Tuple.second



-- Utility functions


{-| Create a nested tuple with one element.
-}
singleton a =
    cons a empty


{-| Map over the head of a nested tuple.
-}
mapHead f tuple =
    cons
        (f (head tuple))
        (tail tuple)


{-| Map over the tail of a nested tuple.
-}
mapTail f tuple =
    cons
        (head tuple)
        (f (tail tuple))


{-| Begin the definition of an `appender`, `mapper`, or `folder`.
-}
define =
    identity



-- Appender


{-| Build up a nested tuple by appending values to the end of it.

This needs to be used in conjunction with `define` and `endAppender`:

    define
        |> appender 1
        |> endAppender

    --> ( 1, () )

    define
        |> appender "hello"
        |> appender "world"
        |> endAppender

    --> ( "hello", ( "world", () ) )

-}
appender value prev next =
    prev ( value, next )


{-| Complete the definition of an `appender`.
-}
endAppender prev =
    prev empty



-- Folders


{-| Create a folder for a nested tuple by defining fold functions for each element of the tuple.

This needs to be used in conjunction with `define` and `endFolder`:

    sum =
        define
            |> folder (\int acc -> toFloat int + acc)
            |> folder (\float acc -> float + acc)
            |> endFolder

    sum 0 ( 1, ( 1.5, () ) )

    --> 2.5

-}
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


{-| Complete the definition of a `folder`.
-}
endFolder =
    end (\acc _ -> acc)


{-| Create a folder for two nested tuples by defining fold functions for each element of the tuples.

This needs to be used in conjunction with `define` and `endFolder2`:

    sum =
        define
            |> folder2 (\int1 int2 acc -> toFloat (int1 + int2) + acc)
            |> folder2 (\float1 float2 acc -> float1 + float2 + acc)
            |> endFolder2

    sum 0
        ( 1, ( 1.5, () ) )
        ( 2, ( 2.5, () ) )

    --> 7

-}
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


{-| Complete the definition of a `folder2`.
-}
endFolder2 =
    end (\acc _ _ -> acc)


{-| Create a folder for three nested tuples by defining fold functions for each element of the tuples.

This needs to be used in conjunction with `define` and `endFolder3`:

    sum =
        define
            |> folder3 (\int1 int2 int3 acc -> toFloat (int1 + int2 + int3) + acc)
            |> folder3 (\float1 float2 float3 acc -> float1 + float2 + float3 + acc)
            |> endFolder2

    sum 0
        ( 1, ( 1.5, () ) )
        ( 2, ( 2.5, () ) )
        ( 3, ( 3.5, () ) )

    --> 13.5

-}
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


{-| Complete the definition of a `folder3`.
-}
endFolder3 =
    end (\acc _ _ _ -> acc)



-- Mappers


{-| Create a `mapper` for a nested tuple by defining a map functions for each element of the tuple.

This needs to be used in conjunction with `define` and `endMapper`:

    double =
        define
            |> mapper (\int -> int * 2)
            |> mapper (\float -> float + float)
            |> endMapper

    double ( 1, ( 1.5, () ) )

    --> ( 2, ( 3.0, () ) )

-}
mapper =
    let
        mapper_ fHead fTail a =
            cons
                (fHead (head a))
                (fTail (tail a))
    in
    do mapper_


{-| Complete the definition of a `mapper`.
-}
endMapper =
    end (\_ -> empty)


{-| Create a `mapper` for two nested tuples by defining map functions for each element of the tuples.

This needs to be used in conjunction with `define` and `endMapper2`:

    add =
        define
            |> mapper2 (\int1 int2 -> int1 + int2)
            |> mapper2 (\float1 float2 -> float1 + float2)
            |> endMapper2

    add ( 1, ( 1.5, () ) )
        ( 2, ( 2.5, () ) )

    --> ( 3, ( 4.0, () ) )

-}
mapper2 =
    let
        mapper2_ fHead fTail a b =
            cons
                (fHead (head a) (head b))
                (fTail (tail a) (tail b))
    in
    do mapper2_


{-| Complete the definition of a `mapper2`.
-}
endMapper2 =
    end (\_ _ -> empty)


{-| Create a `mapper` for three nested tuples by defining map functions for each element of the tuples.

This needs to be used in conjunction with `define` and `endMapper3`:

    add =
        define
            |> mapper3 (\int1 int2 int3 -> int1 + int2 + int3)
            |> mapper3 (\float1 float2 float3 -> float1 + float2 + float3)
            |> endMapper3

    add ( 1, ( 1.5, () ) )
        ( 2, ( 2.5, () ) )
        ( 3, ( 3.5, () ) )

    --> ( 6, ( 7.5, () ) )

-}
mapper3 =
    let
        mapper3_ fHead fTail a b c =
            cons
                (fHead (head a) (head b) (head c))
                (fTail (tail a) (tail b) (tail c))
    in
    do mapper3_


{-| Complete the definition of a `mapper3`.
-}
endMapper3 =
    end (\_ _ _ -> empty)



-- Getters and setters


{-| Begin the definition of a tuple of `getters`.
-}
defineGetters =
    { get = identity
    , getters = identity
    }


{-| Create a tuple of `getters` for a nested tuple.

This needs to be used in conjunction with `defineGetters` and `endGetters`:

    defineGetters
        |> getter
        |> getter
        |> endGetters

    --> ( \tuple -> tuple |> Tuple.first, ( \tuple -> tuple |> Tuple.second |> Tuple.first, () ) )

-}
getter prev =
    { get = prev.get << tail
    , getters = prev.getters << cons (prev.get << head)
    }


{-| Complete the definition of a tuple of `getters`.
-}
endGetters prev =
    prev.getters empty


{-| Begin the definition of a tuple of `setters`.
-}
defineSetters =
    { set = identity
    , setters = identity
    }


{-| Create a tuple of `setters` for a nested tuple.

This needs to be used in conjunction with `defineSetters` and `endSetters`:

    defineSetters
        |> setter
        |> setter
        |> endSetters

    --> ( \value tuple -> tuple |> Tuple.mapFirst (always value), ( \value tuple -> tuple |> Tuple.mapSecond |> Tuple.mapFirst (always value), () ) )

-}
setter prev =
    { set = prev.set << mapTail
    , setters = prev.setters << cons (prev.set << mapHead << always)
    }


{-| Complete the definition of a tuple of `setters`.
-}
endSetters prev =
    prev.setters empty



-- Magic


do doer doThis doPrev =
    \doRest -> doPrev (doer doThis doRest)


end ender prev =
    prev ender
