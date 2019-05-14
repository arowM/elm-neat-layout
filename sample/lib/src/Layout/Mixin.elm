module Layout.Mixin exposing
    ( alignBaseline
    , alignCenter
    , alignEnd
    , alignStart
    , alignStretch
    , expanded
    , flexNowrap
    , fullHeight
    , fullWidth
    , justifyAround
    , justifyCenter
    , justifyEnd
    , justifyEvenly
    , justifyStart
    , onPC
    , onSP
    , onTablet
    , row
    )

{-| Mixins for layout.

@docs alignBaseline
@docs alignCenter
@docs alignEnd
@docs alignStart
@docs alignStretch
@docs expanded
@docs flexNowrap
@docs fullHeight
@docs fullWidth
@docs justifyAround
@docs justifyCenter
@docs justifyEnd
@docs justifyEvenly
@docs justifyStart
@docs onPC
@docs onSP
@docs onTablet
@docs row

-}

import Css
import Mixin exposing (Mixin)


{-| -}
row : Mixin msg
row =
    class "row"


{-| -}
justifyStart : Mixin msg
justifyStart =
    class "justifyStart"


{-| -}
justifyEnd : Mixin msg
justifyEnd =
    class "justifyEnd"


{-| -}
justifyCenter : Mixin msg
justifyCenter =
    class "justifyCenter"


{-| -}
justifyAround : Mixin msg
justifyAround =
    class "justifyAround"


{-| -}
justifyEvenly : Mixin msg
justifyEvenly =
    class "justifyEvenly"


{-| -}
alignStretch : Mixin msg
alignStretch =
    class "alignStretch"


{-| -}
alignStart : Mixin msg
alignStart =
    class "alignStart"


{-| -}
alignEnd : Mixin msg
alignEnd =
    class "alignEnd"


{-| -}
alignCenter : Mixin msg
alignCenter =
    class "alignCenter"


{-| -}
alignBaseline : Mixin msg
alignBaseline =
    class "alignBaseline"


{-| -}
flexNowrap : Mixin msg
flexNowrap =
    class "flexNowrap"


{-| -}
expanded : Mixin msg
expanded =
    class "expanded"


{-| -}
fullWidth : Mixin msg
fullWidth =
    class "fullWidth"


{-| -}
fullHeight : Mixin msg
fullHeight =
    class "fullHeight"


{-| -}
onPC : Mixin msg
onPC =
    class "onPC"


{-| -}
onTablet : Mixin msg
onTablet =
    class "onTablet"


{-| -}
onSP : Mixin msg
onSP =
    class "onSP"



-- Helper functions


{-| A specialized version of `class` for this module.
It handles generated class name by CSS modules.
-}
class : String -> Mixin msg
class str =
    Mixin.fromAttributes
        [ Css.classWithPrefix "layout__" str
        ]
