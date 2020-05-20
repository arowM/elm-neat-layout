module Repl.Ast.Gap exposing
    ( AstGap(..)
    , GapSize
    , Error(..)
    , isInvalid
    , mappend
    , reduceChildGaps
    )

{-| Gap representation for AST.


# Core

@docs AstGap
@docs GapSize
@docs Error
@docs isInvalid


# Gap calculations

@docs mappend
@docs reduceChildGaps

-}

-- Core


{-| Main type
-}
type AstGap
    = NoGap
    | Gap String GapSize
    | Invalid Error
    | Undetermined


type CustomGap
    = GustomGap String GapSize


{-| Size definition for a Gap
-}
type alias GapSize =
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
isInvalid : AstGap -> Bool
isInvalid gap =
    case gap of
        Invalid _ ->
            True

        _ ->
            False



-- Calculations


{-| -}
mappend : AstGap -> AstGap -> AstGap
mappend g1 g2 =
    case ( g1, g2 ) of
        ( Invalid _, _ ) ->
            Invalid InheritedError

        ( g, Undetermined ) ->
            g

        ( _, g ) ->
            g


{-| -}
reduceChildGaps : List AstGap -> AstGap
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
