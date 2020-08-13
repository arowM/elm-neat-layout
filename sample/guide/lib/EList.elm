module EList exposing
    ( EList
    , Key
    , fromList
    , toList
    , toKeyedList
    , empty
    , singleton
    , length
    , map
    , keyedMap
    , indexedMap
    , zipWithList
    , concat
    , find
    , append
    , insertBefore
    , insertAfter
    , swap
    , remove
    , update
    , modify
    , moveBefore
    , moveAfter
    , uniqueString
    )

{-|

# Core

@docs EList
@docs Key
@docs fromList
@docs toList
@docs toKeyedList
@docs length

# Special values

@docs empty
@docs singleton

# Transform

@docs map
@docs keyedMap
@docs indexedMap
@docs zipWithList
@docs concat
@docs append

# Operators

@docs find
@docs insertBefore
@docs insertAfter
@docs swap
@docs remove
@docs update
@docs modify
@docs moveBefore
@docs moveAfter

# Lower level functions

@docs uniqueString

-}


{-| -}
type EList a
    = EList
        { next : Int
        , list : List ( Int, a )
        }


{-| The `Key` is used to specify the target when editing an element of `EList`.
-}
type Key
    = Key Int


{-| -}
fromList : List a -> EList a
fromList ls =
    EList
        { next = List.length ls
        , list = List.indexedMap (\n a -> ( n, a )) ls
        }


{-| -}
toList : EList a -> List a
toList (EList o) =
    List.map Tuple.second o.list


{-| -}
toKeyedList : EList a -> List ( Key, a )
toKeyedList (EList o) =
    List.map (\( n, a ) -> ( Key n, a )) o.list


{-| -}
length : EList a -> Int
length (EList { list }) =
    List.length list


-- Special values

empty : EList a
empty = fromList []


singleton : a -> EList a
singleton a = fromList [a]

-- Transform

{-| -}
map : (a -> b) -> EList a -> EList b
map f (EList o) =
    EList
        { next = o.next
        , list = List.map (\(k, v) -> (k, f v)) o.list
        }


{-| -}
keyedMap : (Key -> a -> b) -> EList a -> EList b
keyedMap f (EList o) =
    EList
        { next = o.next
        , list = List.map (\(k, v) -> (k, f (Key k) v)) o.list
        }


{-| -}
indexedMap : (Int -> a -> b) -> EList a -> EList b
indexedMap f (EList o) =
    EList
        { next = o.next
        , list = List.indexedMap (\n (k, v) -> (k, f n v)) o.list
        }


{-| -}
append : EList a -> EList a -> EList a
append (EList a) (EList b) =
    EList
        { next = a.next + b.next
        , list = a.list ++ List.map (\( n, x ) -> ( n + a.next, x )) b.list
        }

{-|
    EList.toList <| zipWithList [1,2,3,4,5,6,7] (EList.fromList [5,4,3])
    --> [(1,5), (2, 4), (3, 3)]
-}
zipWithList : List a -> EList b -> EList (a, b)
zipWithList ls (EList o) =
    EList
        { next = o.next
        , list = List.map2 (\a (n, b) -> (n, (a, b))) ls (o.list)
        }


{-| -}
concat : EList (EList a) -> EList a
concat (EList o) =
    let
        f : EList a -> EList a -> EList a
        f (EList a) (EList acc) =
            EList
                { next = acc.next + a.next
                , list = List.map (\( n, x ) -> ( n + acc.next, x )) a.list ++ acc.list
                }
    in
    List.foldr
        f
        (EList
            { next = o.next
            , list = []
            }
        )
        (List.map Tuple.second o.list)


-- Operators

{-| -}
find : Key -> EList a -> Maybe a
find (Key k) (EList o) =
    o.list
        |> List.filter (\( n, _ ) -> n == k)
        |> List.head
        |> Maybe.map Tuple.second


{-| Insert an element in front of the element specified by a key.
Return the original list if the key does not exist.
-}
insertBefore : Key -> a -> EList a -> EList a
insertBefore (Key k) a (EList o) =
    EList
        { next =
            if List.any (\( n, _ ) -> n == k) o.list then
                o.next + 1

            else
                o.next
        , list =
            List.foldr
                (\( n, x ) acc ->
                    if n == k then
                        ( o.next, a ) :: ( n, x ) :: acc

                    else
                        ( n, x ) :: acc
                )
                []
                o.list
        }


{-| Insert an element after the element specified by a key.
Return the original list if the key does not exist.
-}
insertAfter : Key -> a -> EList a -> EList a
insertAfter (Key k) a (EList o) =
    EList
        { next =
            if List.any (\( n, _ ) -> n == k) o.list then
                o.next + 1

            else
                o.next
        , list =
            List.foldr
                (\( n, x ) acc ->
                    if n == k then
                        ( n, x ) :: ( o.next, a ) :: acc

                    else
                        ( n, x ) :: acc
                )
                []
                o.list
        }


{-| Swap two values in a list by keys.
Return the original list if the key does not exist.
If the same key is supplied twice the operation has no effect.
-}
swap : Key -> Key -> EList a -> EList a
swap (Key k1) (Key k2) (EList o) =
    let
        ( p1, t1 ) =
            splitAt k1 o.list

        ( h2, t2 ) =
            splitAt k2 t1
    in
    case ( h2, t2 ) of
        ( [], [] ) ->
            EList o

        ( _, [] ) ->
            swap (Key k2) (Key k1) (EList o)

        ( [], _ ) ->
            EList o

        ( v1 :: p2, v2 :: p3 ) ->
            EList
                { next = o.next
                , list =
                    List.concat
                        [ p1, v2 :: p2, v1 :: p3 ]
                }


splitAt : Int -> List ( Int, a ) -> ( List ( Int, a ), List ( Int, a ) )
splitAt k =
    List.foldr
        (\( n, x ) ( p1, p2 ) ->
            if p2 == [] then
                if n == k then
                    ( [], ( n, x ) :: p1 )

                else
                    ( ( n, x ) :: p1, p2 )

            else
                ( ( n, x ) :: p1, p2 )
        )
        ( [], [] )


{-| Remove a value in a list by a key.
Return the original list if the key does not exist.
-}
remove : Key -> EList a -> EList a
remove (Key k) (EList o) =
    EList
        { next = o.next
        , list = List.filter (\( n, _ ) -> n /= k) o.list
        }


{-| Overwrite the value in a list specified by a key.
Return the original list if the key does not exist.
-}
update : Key -> a -> EList a -> EList a
update (Key k) a (EList o) =
    EList
        { next = o.next
        , list =
            List.foldr
                (\( n, x ) acc ->
                    if n == k then
                        ( n, a ) :: acc

                    else
                        ( n, x ) :: acc
                )
                []
                o.list
        }


{-| Modify the value in a list specified by a key.
Return the original list if the key does not exist.
-}
modify : Key -> (a -> a) -> EList a -> EList a
modify (Key k) f (EList o) =
    EList
        { next = o.next
        , list =
            List.foldr
                (\( n, x ) acc ->
                    if n == k then
                        ( n, f x ) :: acc

                    else
                        ( n, x ) :: acc
                )
                []
                o.list
        }


{-| Move a value in front of the value in a list specified by a key.
The first key specifies the element to be moved.
The element is moved in front of the value specified by the second key.
Return the original list if the key does not exist.
If the same key is supplied twice the operation has no effect.
-}
moveBefore : Key -> Key -> EList a -> EList a
moveBefore (Key from) (Key to) (EList o) =
    case ( find (Key from) (EList o), find (Key to) (EList o) ) of
        ( Nothing, _ ) ->
            EList o

        ( _, Nothing ) ->
            EList o

        ( Just moved, _ ) ->
            EList
                { next = o.next
                , list =
                    List.foldr
                        (\( n, x ) acc ->
                            if n == to then
                                ( from, moved ) :: ( n, x ) :: acc

                            else
                                ( n, x ) :: acc
                        )
                        []
                        (List.filter (\( n, _ ) -> n /= from) o.list)
                }


{-| Move a value after the value in a list specified by a key.
The first key specifies the element to be moved.
The element is moved after the value specified by the second key.
Return the original list if the key does not exist.
If the same key is supplied twice the operation has no effect.
-}
moveAfter : Key -> Key -> EList a -> EList a
moveAfter (Key from) (Key to) (EList o) =
    case ( find (Key from) (EList o), find (Key to) (EList o) ) of
        ( Nothing, _ ) ->
            EList o

        ( _, Nothing ) ->
            EList o

        ( Just moved, _ ) ->
            EList
                { next = o.next
                , list =
                    List.foldr
                        (\( n, x ) acc ->
                            if n == to then
                                ( n, x ) :: ( from, moved ) :: acc

                            else
                                ( n, x ) :: acc
                        )
                        []
                        (List.filter (\( n, _ ) -> n /= from) o.list)
                }


{-| Generates a unique string for `Html.Keyed`.
-}
uniqueString : Key -> String
uniqueString (Key k) =
    String.fromInt k
