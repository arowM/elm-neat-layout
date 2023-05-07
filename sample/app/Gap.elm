module Gap exposing
    ( Body
    , body
    , Sub
    , sub
    )

{-|

@docs Body
@docs body
@docs Sub
@docs sub

-}

import Neat exposing (IsGap)


{-| -}
type Body
    = Body Never


{-| -}
body : IsGap Body
body =
    Neat.customGap
        { vertical = 0.6
        , horizontal = 0.6
        }


{-| -}
type Sub
    = Sub Never


{-| -}
sub : IsGap Sub
sub =
    Neat.customGap
        { vertical = 0.3
        , horizontal = 0.3
        }
