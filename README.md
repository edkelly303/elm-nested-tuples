# Elm Nested Tuples

Work with nested tuples of the form `( a, ( b, ( c, () ) ) )`.

For example:

```elm
import NestedTuple exposing (cons, empty, define, mapper, endMapper, folder, endFolder)

myTuple = 
    cons 1 (cons "hello" empty)

myTuple

--> ( 1, ( "hello", () ) )

myMapper = 
    define
        |> mapper (\int -> int * 2)
        |> mapper (\str -> str ++ " world")
        |> endMapper

myMapper myTuple

--> ( 2, ( "hello world", () ) )

myFolder =     
    define
        |> folder (\int acc -> int * 2 + acc)
        |> folder (\str acc -> String.length str + acc)
        |> endFolder

myFolder 0 myTuple

--> 7
```