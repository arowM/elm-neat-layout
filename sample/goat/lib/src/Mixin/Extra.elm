module Mixin.Extra exposing (boolAttribute)

{-| Helper mixins.


# Custom Attributes

@docs boolAttribute

-}

import Mixin exposing (Mixin)
import Html.Attributes as Attributes



-- Custom Attributes


{-| Create arbitrary string properties.
-}
boolAttribute : String -> Bool -> Mixin msg
boolAttribute name b =
    Mixin.fromAttribute <| Attributes.attribute name <|
        if b then
            "true"

        else
            "false"
