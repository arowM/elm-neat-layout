module Css exposing (classWithPrefix)

{-| Helper functions related to CSS.


# CSS modules

@docs classWithPrefix

-}

import Html.Attributes as Attributes
import Mixin exposing (Mixin)


{-| Helper function to declare module specific `class` function which prepend prefix to each class names.
In CSS files, the prefix is supposed to be prepend to each class selectors by CSS modules.
-}
classWithPrefix : String -> String -> Mixin msg
classWithPrefix prefix =
    Mixin.fromAttributes << List.map (\name -> Attributes.class <| prefix ++ name) << String.split " "
