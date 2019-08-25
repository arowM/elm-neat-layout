# elm-neat-layout

Elm layout framework that helps to keep paddings neat.

## What's elm-neat-layout?

The elm-neat-layout is a layout framework.

## Sample

Check sample/bleater of this repository.

```elm
menuItem : MenuItem -> View NoPadding msg
menuItem m =
    Layout.rowWith
        { defaultRow
            | vertical = Row.Bottom
        }
        [ Neat.div
            [ class <| MenuItem.toClassname m
            , class "menuItem_icon"
            ]
            []
            |> fromNoPadding iconPadding
        , Neat.div
            [ class "menuItem_label"
            ]
            [ Neat.text <| MenuItem.toLabel m
            ]
            |> fromNoPadding iconPadding
            |> setLayout Layout.fill
        ]
        |> setBoundary iconPadding
        |> setClass "menuItem"
```
