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
the form:

`( a, ( b, ( c, () ) ) )`.


# Primitives

@docs empty, cons, head, tail

**Note:** From the four primitive functions `empty`, `cons`, `head` and `tail`, all the
others can be derived.

In your own project, you might want to represent a nested tuple as something
other than a tuple, for example:

  - `type alias MyTuple a b = { head : a, tail : b }`
  - `type MyTuple a b = MyTuple a b`

If so, you could just vendor this package and redefine these four functions
for your preferred data type, then delete all the type annotations, and everything
else should _just work_.


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
empty : ()
empty =
    ()


{-| Prepend a value to a nested tuple.
-}
cons : head -> tail -> ( head, tail )
cons =
    Tuple.pair


{-| Get the head (first element) of a nested tuple.
-}
head : ( head, tail ) -> head
head =
    Tuple.first


{-| Get the tail (everything except the first element) of a nested tuple.
-}
tail : ( head, tail ) -> tail
tail =
    Tuple.second



-- Utility functions


{-| Create a nested tuple with one element.
-}
singleton : head -> ( head, () )
singleton a =
    cons a empty


{-| Map over the head of a nested tuple.
-}
mapHead : (headA -> headB) -> ( headA, tail ) -> ( headB, tail )
mapHead f tuple =
    cons
        (f (head tuple))
        (tail tuple)


{-| Map over the tail of a nested tuple.
-}
mapTail : (tailA -> tailB) -> ( head, tailA ) -> ( head, tailB )
mapTail f tuple =
    cons
        (head tuple)
        (f (tail tuple))


{-| Begin the definition of an `appender`, `mapper`, or `folder`.
-}
define : a -> a
define =
    identity



-- Appender


{-| Build up a nested tuple by appending values to the end of it.

This needs to be used in conjunction with `define` and `endAppender`:

    import NestedTuple exposing (define, appender, endAppender)

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
appender : head -> (( head, tail ) -> tuple) -> tail -> tuple
appender value prev next =
    prev (cons value next)


{-| Complete the definition of an `appender`.
-}
endAppender : (() -> tuple) -> tuple
endAppender prev =
    prev empty



-- Mappers


{-| Create a `mapper` for a nested tuple by defining a map functions for each element of the tuple.

This needs to be used in conjunction with `define` and `endMapper`:

    import NestedTuple exposing (define, mapper, endMapper)

    double =
        define
            |> mapper (\int -> int * 2)
            |> mapper (\float -> float + float)
            |> endMapper

    double ( 1, ( 1.5, () ) )

    --> ( 2, ( 3.0, () ) )

-}
mapper :
    (headA -> headB)
    -> ((( headA, tailA ) -> ( headB, tailB )) -> tuple)
    -> (tailA -> tailB)
    -> tuple
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
endMapper : ((() -> ()) -> tuple) -> tuple
endMapper =
    end (\_ -> empty)


{-| Create a `mapper` for two nested tuples by defining map functions for each element of the tuples.

This needs to be used in conjunction with `define` and `endMapper2`:

    import NestedTuple exposing (define, mapper2, endMapper2)

    add =
        define
            |> mapper2 (\int1 int2 -> int1 + int2)
            |> mapper2 (\float1 float2 -> float1 + float2)
            |> endMapper2

    add ( 1, ( 1.5, () ) )
        ( 2, ( 2.5, () ) )

    --> ( 3, ( 4.0, () ) )

-}
mapper2 :
    (headA -> headB -> headC)
    -> ((( headA, tailA ) -> ( headB, tailB ) -> ( headC, tailC )) -> tuple)
    -> (tailA -> tailB -> tailC)
    -> tuple
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
endMapper2 : ((() -> () -> ()) -> tuple) -> tuple
endMapper2 =
    end (\_ _ -> empty)


{-| Create a `mapper` for three nested tuples by defining map functions for each element of the tuples.

This needs to be used in conjunction with `define` and `endMapper3`:

    import NestedTuple exposing (define, mapper3, endMapper3)

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
mapper3 :
    (headA -> headB -> headC -> headD)
    -> ((( headA, tailA ) -> ( headB, tailB ) -> ( headC, tailC ) -> ( headD, tailD )) -> tuple)
    -> (tailA -> tailB -> tailC -> tailD)
    -> tuple
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
endMapper3 : ((() -> () -> () -> ()) -> tuple) -> tuple
endMapper3 =
    end (\_ _ _ -> empty)



-- Folders


{-| Create a `folder` for a nested tuple by defining fold functions for each element of the tuple.

This needs to be used in conjunction with `define` and `endFolder`:

    import NestedTuple exposing (define, folder, endFolder)

    sum =
        define
            |> folder (\int acc -> int + acc)
            |> folder (\str acc -> String.length str + acc)
            |> endFolder

    sum 0 ( 1, ( "foo", () ) )

    --> 4

-}
folder :
    (head -> accForHead -> accForTail)
    -> ((accForHead -> ( head, tail ) -> accForNext) -> folder)
    -> (accForTail -> tail -> accForNext)
    -> folder
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
endFolder : ((acc -> empty -> acc) -> folder) -> folder
endFolder =
    end (\acc _ -> acc)


{-| Create a `folder2` for two nested tuples by defining fold functions for each element of the tuples.

This needs to be used in conjunction with `define` and `endFolder2`:

    import NestedTuple exposing (define, folder2, endFolder2)

    sum =
        define
            |> folder2 (\int1 int2 acc -> int1 + int2 + acc)
            |> folder2 (\str1 str2 acc -> String.length (str1 ++ str2) + acc)
            |> endFolder2

    sum 0
        ( 1, ( "foo", () ) )
        ( 2, ( "bar", () ) )

    --> 9

-}
folder2 :
    (headA -> headB -> accForHead -> accForTail)
    -> ((accForHead -> ( headA, tailA ) -> ( headB, tailB ) -> accForNext) -> folder2)
    -> (accForTail -> tailA -> tailB -> accForNext)
    -> folder2
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
endFolder2 : ((acc -> empty -> empty -> acc) -> folder2) -> folder2
endFolder2 =
    end (\acc _ _ -> acc)


{-| Create a `folder3` for three nested tuples by defining fold functions for each element of the tuples.

This needs to be used in conjunction with `define` and `endFolder3`:

    import NestedTuple exposing (define, folder3, endFolder3)

    sum =
        define
            |> folder3 (\int1 int2 int3 acc -> int1 + int2 + int3 + acc)
            |> folder3 (\str1 str2 str3 acc -> String.length (str1 ++ str2 ++ str3) + acc)
            |> endFolder3

    sum 0
        ( 1, ( "foo", () ) )
        ( 2, ( "bar", () ) )
        ( 3, ( "baz", () ) )

    --> 15

-}
folder3 :
    (headA -> headB -> headC -> accForHead -> accForTail)
    -> ((accForHead -> ( headA, tailA ) -> ( headB, tailB ) -> ( headC, tailC ) -> accForNext) -> folder3)
    -> (accForTail -> tailA -> tailB -> tailC -> accForNext)
    -> folder3
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
endFolder3 : ((acc -> empty -> empty -> empty -> acc) -> folder3) -> folder3
endFolder3 =
    end (\acc _ _ _ -> acc)



-- Getters and setters


{-| Begin the definition of a tuple of `getters`.
-}
defineGetters :
    { focus : focus -> focus
    , appendToGetters : getters -> getters
    }
defineGetters =
    { focus = identity
    , appendToGetters = identity
    }


{-| Create a tuple of `getters` for a nested tuple.

This needs to be used in conjunction with `defineGetters` and `endGetters`:

    import NestedTuple exposing (defineGetters, getter, endGetters)

    defineGetters
        |> getter
        |> getter
        |> endGetters

    --: ( ( a, ( b, () ) ) -> a
        , ( ( a, ( b, () ) ) -> b
          , () 
          ) 
        )

-}
getter :
    { appendToGetters : ( tuple -> head, nextGetters ) -> getters
    , focus : tuple -> ( head, tail )
    }
    ->
        { appendToGetters : nextGetters -> getters
        , focus : tuple -> tail
        }
getter { focus, appendToGetters } =
    let
        nextFocus tuple =
            tail (focus tuple)

        thisGetter tuple =
            head (focus tuple)
    in
    { focus = nextFocus
    , appendToGetters = \nextGetter -> appendToGetters (cons thisGetter nextGetter)
    }


{-| Complete the definition of a tuple of `getters`.
-}
endGetters :
    { focus : focus
    , appendToGetters : () -> getters
    }
    -> getters
endGetters { appendToGetters } =
    appendToGetters empty


{-| Begin the definition of a tuple of `setters`.
-}
defineSetters :
    { focus : focus -> focus
    , appendToSetters : setters -> setters
    }
defineSetters =
    { focus = identity
    , appendToSetters = identity
    }


{-| Create a tuple of `setters` for a nested tuple.

This needs to be used in conjunction with `defineSetters` and `endSetters`:

    import NestedTuple exposing (defineSetters, setter, endSetters)

    defineSetters
        |> setter
        |> setter
        |> endSetters

    --: ( a -> ( a, ( b, () ) ) -> ( a, ( b, () ) )
        , ( b -> ( a, ( b, () ) ) -> ( a, ( b, () ) )
          , () 
          ) 
        )

-}
setter :
    { appendToSetters : ( head -> tuple -> tuple, nextSetters ) -> setters
    , focus : (( head, tail ) -> ( head, tail )) -> tuple -> tuple
    }
    ->
        { appendToSetters : nextSetters -> setters
        , focus : (tail -> tail) -> tuple -> tuple
        }
setter { focus, appendToSetters } =
    let
        nextFocus f tuple =
            focus (mapTail f) tuple

        thisSetter value tuple =
            focus (mapHead (always value)) tuple
    in
    { focus = nextFocus
    , appendToSetters = \nextSetter -> appendToSetters (cons thisSetter nextSetter)
    }


{-| Complete the definition of a tuple of `setters`.
-}
endSetters :
    { focus : focus
    , appendToSetters : () -> setters
    }
    -> setters
endSetters { appendToSetters } =
    appendToSetters empty



-- Magic


do doer doThis doPrev =
    \doRest -> doPrev (doer doThis doRest)


end ender prev =
    prev ender
