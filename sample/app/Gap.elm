module Gap exposing
    ( Body
    , body
    )

{-|

@docs Body
@docs body

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
