module Main exposing (Model, Msg, main)

import Browser exposing (Document)
import Gap
import Mixin
import Neat
import Neat.Boundary as Boundary exposing (Boundary)
import Neat.View as View



-- App


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    ()


init : ( Model, Cmd Msg )
init =
    ( ()
    , Cmd.none
    )


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    { title = "Sample"
    , body =
        [ body model
            |> Neat.render Neat.defaultRenderer
        ]
    }


body : Model -> Boundary Msg
body _ =
    View.column
        (View.defaultColumn
            |> View.alignBottom
        )
        [ View.row
            View.defaultRow
            [ Boundary.textBlock "Header"
                |> Boundary.setGap Gap.body
                |> View.grownMiddleItem "text"
            , Boundary.empty
                |> Boundary.setMinWidthInEm 4
                |> Boundary.setMixin (Mixin.attribute "placeholder" "foo")
                |> Boundary.setMixin (Mixin.style "padding" "0.4em")
                |> Boundary.setGap Gap.body
                |> View.setNodeName "input"
                |> View.grownMiddleItem "input"
            , View.row
                (View.defaultRow
                    |> View.alignCenter
                )
                [ Boundary.textBlock "三"
                    |> Boundary.setGap Neat.noGap
                    |> View.middleItem "icon"
                ]
                |> View.setBoundary
                |> Boundary.setMinWidthInEm 3
                |> Boundary.setMaxWidthInEm 3
                |> Boundary.setMinHeightInEm 3
                |> Boundary.setMaxHeightInEm 3
                |> Boundary.setMixin (Mixin.class "header_hamburger")
                |> Boundary.setGap Gap.body
                |> View.middleItem "hamburger"
            ]
            |> View.setBoundary
            |> Boundary.setMixin (Mixin.class "header")
            |> Boundary.setGap Neat.noGap
            |> View.columnItem "header"
        , View.column
            View.defaultColumn
            [ Boundary.textBlock
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                -- |> Boundary.setMinHeightInEm 10
                |> Boundary.setMaxHeightInEm 25
                -- |> Boundary.setMaxWidthInEm 35
                |> Boundary.setMinWidthInEm 16
                |> Boundary.setGap Gap.body
                |> View.columnItem "sampleText"
            , Boundary.empty
                |> Boundary.setMinHeightInEm 5
                |> Boundary.setMinWidthInEm 10
                |> Boundary.setMaxWidthInEm 20
                |> Boundary.setMixin (Mixin.class "red")
                |> Boundary.setGap Gap.body
                |> View.columnItem "sampleBox"
            , View.row
                (View.defaultRow
                    |> View.alignRight
                )
                [ Boundary.empty
                    |> Boundary.setMinHeightInEm 5
                    |> Boundary.setMinWidthInEm 12
                    |> Boundary.setMaxWidthInEm 23
                    |> Boundary.setMixin (Mixin.class "blue")
                    |> Boundary.setGap Gap.body
                    |> View.grownRowItem "sampleBox"
                ]
                |> View.setBoundary
                |> Boundary.setMixin (Mixin.class "red")
                |> Boundary.setMinWidthInEm 37
                |> Boundary.setMaxWidthInEm 40
                |> Boundary.setMaxHeightInEm 33
                |> Boundary.putLayer "overlay"
                    ( { top = 50
                      , bottom = 0
                      , left = 0
                      , right = 50
                      , priority = Nothing
                      }
                    , Boundary.empty
                        |> Boundary.setMaxWidthInEm 20
                        |> Boundary.setMixin (Mixin.class "red")
                        |> Boundary.toLayered
                    )
                |> Boundary.setGap Gap.body
                |> View.grownCenterItem "sampleNestedBox"
            ]
            |> View.setBoundary
            |> Boundary.setMinHeightInEm 10
            |> Boundary.enableVerticalScroll
            |> Boundary.setMixin (Mixin.class "blue")
            |> Boundary.setGap Neat.noGap
            |> View.grownColumnItem "body"
        , Boundary.textBlock "Footer"
            |> Boundary.setGap Gap.body
            |> View.setBoundary
            |> Boundary.setMixin (Mixin.class "footer")
            |> Boundary.setGap Neat.noGap
            |> View.columnItem "footer"
        ]
        |> View.setBoundary


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
