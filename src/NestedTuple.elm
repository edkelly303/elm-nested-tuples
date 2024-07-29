module NestedTuple exposing (..)


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
defineFold =
    identity


fold =
    let
        folder foldThis_ foldRest_ acc ( this, rest ) =
            foldRest_ (foldThis_ this acc) rest
    in
    do folder


endFold =
    end (\acc () -> acc)


defineMap =
    identity


map =
    let
        mapper mapThis_ mapRest_ ( this, rest ) =
            ( mapThis_ this
            , mapRest_ rest
            )
    in
    do mapper


endMap =
    end (\() -> ())


m =
    defineMap
        |> map (\n -> n * 2)
        |> map (\s -> s ++ s)
        |> endMap


tup =
    ( 1, ( "hello", () ) )
