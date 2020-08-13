module Repl.AccumGap exposing
    ( AccumGap(..)
    , Size
    , Error(..)
    , isInvalid
    , mappend
    , reduceChildGaps
    )

{-| Accumulated gaps for rendering AST.


# Core

@docs AccumGap
@docs Size
@docs Error
@docs isInvalid


# Gap calculations

@docs mappend
@docs reduceChildGaps

-}

-- Core


{-| Main type
-}
type AccumGap
    = NoGap
    | Gap String Size
    | Invalid Error
    | Undetermined


{-| Size definition for a Gap
-}
type alias Size =
    { width : Float
    , height : Float
    }


{-| Error types on calculating Gaps
-}
type Error
    = ChildGapMismatch
    | InheritedError


{-| Checks whether a Gap is `Invalid _`.
-}
isInvalid : AccumGap -> Bool
isInvalid gap =
    case gap of
        Invalid _ ->
            True

        _ ->
            False



-- Calculations


{-| -}
mappend : AccumGap -> AccumGap -> AccumGap
mappend g1 g2 =
    case ( g1, g2 ) of
        ( Invalid _, _ ) ->
            Invalid InheritedError

        ( g, Undetermined ) ->
            g

        ( _, g ) ->
            g


{-| -}
reduceChildGaps : List AccumGap -> AccumGap
reduceChildGaps gs =
    case List.filter (\a -> a /= Undetermined) gs of
        [] ->
            Undetermined

        x :: xs ->
            if List.all ((==) x) xs then
                x

            else if List.any isInvalid gs then
                Invalid InheritedError

            else
                Invalid ChildGapMismatch
