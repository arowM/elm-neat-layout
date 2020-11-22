module Neat.Size exposing
    ( Size
    , width
    , minWidth
    , maxWidth
    , height
    , minHeight
    , maxHeight
    )

{-| Size functions.


# Core

@docs Size


# Width

@docs width
@docs minWidth
@docs maxWidth


# Height

@docs height
@docs minHeight
@docs maxHeight

-}

import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Neat.Layout.Internal as Layout
import Neat.Size.Internal as Size



-- Core


{-| -}
type alias Size msg =
    Size.Size msg



-- Width


{-| -}
width : String -> Size msg
width v =
    Size.Size <|
        \renderer gap ->
            Layout.fromRecord
                { outer =
                    Mixin.batch
                        [ style "width" <|
                            "calc("
                                ++ v
                                ++ " + "
                                ++ String.fromFloat gap.width
                                ++ " * 2 * "
                                ++ renderer.baseGapSize
                                ++ ")"
                        ]
                , inner =
                    Mixin.batch
                        [ style "width" "100%"
                        ]
                }
                |> Layout.makeImportant


{-| -}
minWidth : String -> Size msg
minWidth v =
    Size.Size <|
        \renderer gap ->
            Layout.fromRecord
                { outer =
                    Mixin.batch
                        [ style "min-width" <|
                            "calc("
                                ++ v
                                ++ " + "
                                ++ String.fromFloat gap.width
                                ++ " * 2 * "
                                ++ renderer.baseGapSize
                                ++ ")"
                        ]
                , inner = Mixin.none
                }
                |> Layout.makeImportant


{-| -}
maxWidth : String -> Size msg
maxWidth v =
    Size.Size <|
        \renderer gap ->
            Layout.fromRecord
                { outer =
                    Mixin.batch
                        [ style "max-width" <|
                            "calc("
                                ++ v
                                ++ " + "
                                ++ String.fromFloat gap.width
                                ++ " * 2 * "
                                ++ renderer.baseGapSize
                                ++ ")"
                        ]
                , inner = Mixin.none
                }
                |> Layout.makeImportant



-- Height


{-| -}
height : String -> Size msg
height v =
    Size.Size <|
        \renderer gap ->
            Layout.fromRecord
                { outer =
                    Mixin.batch
                        [ style "height" <|
                            "calc("
                                ++ v
                                ++ " + "
                                ++ String.fromFloat gap.height
                                ++ " * 2 * "
                                ++ renderer.baseGapSize
                                ++ ")"
                        ]
                , inner =
                    Mixin.batch
                        [ style "height" "100%"
                        ]
                }
                |> Layout.makeImportant


{-| -}
minHeight : String -> Size msg
minHeight v =
    Size.Size <|
        \renderer gap ->
            Layout.fromRecord
                { outer =
                    Mixin.batch
                        [ style "min-height" <|
                            "calc("
                                ++ v
                                ++ " + "
                                ++ String.fromFloat gap.height
                                ++ " * 2 * "
                                ++ renderer.baseGapSize
                                ++ ")"
                        ]
                , inner = Mixin.none
                }
                |> Layout.makeImportant


{-| -}
maxHeight : String -> Size msg
maxHeight v =
    Size.Size <|
        \renderer gap ->
            Layout.fromRecord
                { outer =
                    Mixin.batch
                        [ style "max-height" <|
                            "calc("
                                ++ v
                                ++ " + "
                                ++ String.fromFloat gap.height
                                ++ " * 2 * "
                                ++ renderer.baseGapSize
                                ++ ")"
                        ]
                , inner = Mixin.none
                }
                |> Layout.makeImportant



-- Helper functions


style : String -> String -> Mixin msg
style k v =
    Mixin.fromAttribute <| Attributes.style k v
