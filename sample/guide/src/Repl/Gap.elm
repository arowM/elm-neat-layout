module Repl.Gap exposing
    ( Gap
    , toIsGap
    )

{-| Gap


# Core

@docs Gap
@docs toIsGap

-}

import Neat exposing (IsGap(..))

-- Core


{-| Size definition for a Gap
-}
type alias Gap =
    { name : String
    , width : Float
    , height : Float
    }


toIsGap : Gap -> IsGap gap
toIsGap gap =
    IsGap
        { width = gap.width
        , height = gap.height
        }
