module Css exposing (classWithPrefix)

{-| Helper functions related to CSS.


# CSS modules

@docs classWithPrefix

-}

import Html exposing (Attribute)
import Html.Attributes as Attributes


{-| Helper function to declare module specific `class` function which prepend prefix to each class names.
In CSS files, the prefix is supposed to be prepend to each class selectors by CSS modules.
-}
classWithPrefix : String -> String -> Attribute msg
classWithPrefix prefix =
    Attributes.classList << List.map (\name -> (prefix ++ name, True) ) << String.split " "
