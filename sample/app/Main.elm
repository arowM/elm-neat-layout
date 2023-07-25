module Main exposing (Model, Msg, main)

import Browser exposing (Document)
import Gap
import Mixin
import Neat
import Neat.Boundary as Boundary exposing (Boundary)
import Neat.Text as Text
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
    Boundary.column
        (Boundary.defaultColumn
            |> Boundary.alignBottom
        )
        [ View.row
            View.defaultRow
            [ View.textBlock Gap.body "Header"
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
                [ View.textBlock Gap.body "三"
                    |> View.middleItem "icon"
                ]
                |> View.setBoundary
                |> Boundary.setMixin (Mixin.class "header_hamburger")
                |> Boundary.setGap Gap.body
                |> View.middleItem "hamburger"
            ]
            |> View.setBoundary
            |> Boundary.setMixin (Mixin.class "header")
            |> Boundary.columnItem "header"
        , View.column
            View.defaultColumn
            [ View.fromTexts Gap.sub
                [ Text.fromString "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do "
                , Text.fromString "<eiusmod>"
                    |> Text.setClass "inlineCode"
                    |> Text.setNodeName "code"
                , Text.fromString " tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                ]
                |> View.expandGap Gap.body
                |> View.columnItem "sampleText"
            , View.column
                View.defaultColumn
                [ Boundary.empty
                    |> Boundary.setMinHeightInEm 2
                    |> Boundary.setMinWidthInEm 10
                    |> Boundary.setMaxWidthInEm 20
                    |> Boundary.setMixin (Mixin.class "red")
                    |> Boundary.setGap Gap.sub
                    |> View.columnItem "sampleBox1"
                , Boundary.empty
                    |> Boundary.setMinHeightInEm 2
                    |> Boundary.setMinWidthInEm 7
                    |> Boundary.setMixin (Mixin.class "blue")
                    |> Boundary.setGap Gap.sub
                    |> View.grownColumnItem "sampleBox2"
                ]
                |> View.expandGap Gap.body
                |> View.grownColumnItem "subBoxes"
            , View.column
                View.defaultColumn
                [ View.textBlock Gap.body "Summary"
                    |> View.setNodeName "summary"
                    |> View.columnItem "summary"
                , View.textBlock Gap.body "Foo"
                    |> View.columnItem "body"
                ]
                |> View.setNodeName "details"
                |> View.columnItem "details"
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
            , View.row
                View.defaultRow
                [ View.textBlock Gap.sub "foo000000\nbar\nbar\nbar\nfoo\nbar\nbar\nbar\n"
                    |> View.grownRowItem "content"
                ]
                |> View.setBoundary
                |> Boundary.setVerticalScroller
                |> Boundary.setMinHeightInEm 1
                |> Boundary.setMaxHeightInEm 8
                |> Boundary.setMixin (Mixin.class "scrollableText_content")
                |> Boundary.setGap Gap.body
                |> View.grownLeftItem "content1"
            , View.row
                View.defaultRow
                [ View.textBlock Gap.sub "foo000000\nbar\nbar\nbar"
                    |> View.grownRowItem "content"
                ]
                |> View.expandGap Gap.body
                |> View.setBoundary
                |> Boundary.setVerticalScroller
                |> Boundary.setMinHeightInEm 1
                |> Boundary.setMaxHeightInEm 8
                |> Boundary.setMixin (Mixin.class "scrollableText_content")
                |> Boundary.setGap Gap.body
                |> View.grownColumnItem "content2"
            ]
            |> View.setBoundary
            |> Boundary.setVerticalScroller
            |> Boundary.setMinHeightInEm 10
            |> Boundary.setMixin (Mixin.class "blue")
            |> Boundary.grownColumnItem "body"
        , View.textBlock Gap.body "Footer"
            |> View.setBoundary
            |> Boundary.setMixin (Mixin.class "footer")
            |> Boundary.columnItem "footer"
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
