module Repl.Gap exposing
    ( Gap
    )

{-| Gap


# Core

@docs Gap

-}

-- Core


{-| Size definition for a Gap
-}
type alias Gap =
    { name : String
    , width : Float
    , height : Float
    }
