module Breadcrumb exposing
    ( Breadcrumb
    , identity
    , excavate
    )


{-|
# Core

@docs Breadcrumb
@docs identity
@docs excavate

-}

{-| -}
type alias Breadcrumb root current =
        { current : current
        , set : current -> root -> root
        }


{-| -}
identity : Breadcrumb root root
identity =
    Breadcrumb
        { get = Basics.identity
        , set = always
        }


{-| -}
excavate : (a -> b, b -> a) -> Breadcrumb root a -> Breadcrumb root b
excavate (go, back) (Breadcrumb { get, set }) =
    Breadcrumb
        { get = get >> go
        , set = set << back
        }
