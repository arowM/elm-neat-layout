module Neat.KeyableList exposing
    ( KeyableList
    , fromList
    , fromKeyedList
    , filter
    , any
    , unwrap
    , head
    , map
    , identifiedMap
    )

{-|

@docs KeyableList
@docs fromList
@docs fromKeyedList
@docs filter
@docs any
@docs unwrap
@docs head
@docs map
@docs identifiedMap

-}


{-| -}
type KeyableList a
    = WithKey (List ( String, a ))
    | WithoutKey (List a)


{-| -}
fromList : List a -> KeyableList a
fromList =
    WithoutKey


{-| -}
fromKeyedList : List ( String, a ) -> KeyableList a
fromKeyedList =
    WithKey


{-| Similar to map, but the argument function also takes following identifier.

    * list with keyes: Key
    * list without keyes: Stringified indexes
-}
identifiedMap : (String -> a -> b) -> KeyableList a -> KeyableList b
identifiedMap f keyable =
    case keyable of
        WithKey ls ->
            List.map (\(k, v) -> (k, f k v)) ls
                |> WithKey

        WithoutKey ls ->
            List.indexedMap (\n -> f (String.fromInt n)) ls
                |> WithoutKey


{-| -}
unwrap : (List a -> b) -> (List ( String, a ) -> b) -> KeyableList a -> b
unwrap f g keyable =
    case keyable of
        WithKey ls ->
            g ls

        WithoutKey ls ->
            f ls



{-| -}
filter : (a -> Bool) -> KeyableList a -> KeyableList a
filter p keyable =
    case keyable of
        WithKey ls ->
            List.filter (p << Tuple.second) ls
                |> WithKey

        WithoutKey ls ->
            List.filter p ls
                |> WithoutKey


{-| -}
any : (a -> Bool) -> KeyableList a -> Bool
any p keyable =
    case keyable of
        WithKey ls ->
            List.any (p << Tuple.second) ls

        WithoutKey ls ->
            List.any p ls


{-| -}
map : (a -> b) -> KeyableList a -> KeyableList b
map f keyable =
    case keyable of
        WithKey ls ->
            WithKey <| List.map (Tuple.mapSecond f) ls

        WithoutKey ls ->
            WithoutKey <| List.map f ls


{-| -}
head : KeyableList a -> Maybe a
head keyable =
    case keyable of
        WithKey ls ->
            ls
                |> List.head
                |> Maybe.map Tuple.second

        WithoutKey ls ->
            List.head ls
