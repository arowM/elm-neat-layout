module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, button, div, text)
import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events.Extra as Events
import Html.Lazy as Html
import MenuItem exposing (MenuItem)
import Mixin exposing (Mixin)
import Neat exposing (NoPadding, Protected, View, fromNoPadding, setBoundary, setLayout, setMixin)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (Column, defaultColumn)
import Neat.Layout.Row as Row exposing (defaultRow)
import Padding exposing (BodyPadding, HeaderPadding, IconPadding, MenuPadding, MessagePadding, bodyPadding, headerPadding, iconPadding, menuPadding, messagePadding)



-- App


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { searchText : String
    , messages : List Message
    }


type alias Message =
    { icon : MessageIcon
    , name : String
    , screenName : String
    , id : Int
    }


type MessageIcon
    = MessageIcon1
    | MessageIcon2


messageIconClass : MessageIcon -> String
messageIconClass icon =
    case icon of
        MessageIcon1 ->
            "messageIcon-icon1"

        MessageIcon2 ->
            "messageIcon-icon2"


init : ( Model, Cmd Msg )
init =
    ( { searchText = ""
      , messages =
            [ message1 1
            , message1 2
            , message2 3
            , message1 4
            , message2 5
            , message2 6
            , message1 7
            , message2 8
            , message1 9
            ]
      }
    , Cmd.none
    )


message1 : Int -> Message
message1 n =
    { icon = MessageIcon1
    , name = "Sakura-chan 1st"
    , screenName = "sakura-chan1"
    , id = n
    }


message2 : Int -> Message
message2 n =
    { icon = MessageIcon2
    , name = "Sakura-chan 2nd"
    , screenName = "sakura-chan2"
    , id = n
    }


type Msg
    = ChangeSearchText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSearchText str ->
            ( { model
                | searchText = str
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Layout.rowWith
        { defaultRow
            | vertical = Row.Stretch
            , wrap = False
        }
        [ menu
            |> setLayout Layout.noShrink
        , body model
        , rightPane
        ]
        |> setClass "center"
        |> alignHCenter
        |> setClass "wrapper"
        |> Neat.toPage
        |> Html.div []


alignHCenter : View NoPadding msg -> View NoPadding msg
alignHCenter v =
    Layout.rowWith
        { defaultRow
            | horizontal = Row.HCenter
        }
        [ v ]



-- Menu


menu : View NoPadding msg
menu =
    MenuItem.enum
        |> List.map (fromNoPadding menuPadding << menuItem)
        |> Layout.columnWith
            { defaultColumn
                | horizontal = Column.Stretch
            }
        |> setBoundary menuPadding
        |> setClass "menu"


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



-- Body


body : Model -> View NoPadding msg
body model =
    Layout.columnWith
        { defaultColumn
            | horizontal = Column.Stretch
        }
        [ bodyTitle
        , bodyContent model.messages
            |> setLayout Layout.fill
            |> setClass "scrollAreaY"
        ]


bodyTitle : View NoPadding msg
bodyTitle =
    Neat.text "Home"
        |> Neat.fromNoPadding headerPadding
        |> setBoundary headerPadding
        |> setClass "bodyTitle"


bodyContent : List Message -> View NoPadding msg
bodyContent messages =
    Column.optimized
        (String.fromInt << .id)
        (\m -> Html.lazy4 message_ m.icon m.name m.screenName)
        { defaultColumn
            | horizontal = Column.Stretch
        }
        messages
        |> setClass "bodyContent"


message_ : MessageIcon -> String -> String -> Column -> Html (Protected NoPadding msg)
message_ icon name screenName =
    Column.toProtected <| message icon name screenName


message : MessageIcon -> String -> String -> View NoPadding msg
message icon name screenName =
    Layout.row
        [ Neat.empty
            |> setClass "messageIcon"
            -- (Debug.log "This functions is only rendered at first" "messageIcon")
            |> setClass (messageIconClass icon)
            |> fromNoPadding bodyPadding
        , Layout.column
            [ Layout.row
                [ Neat.text name
                    |> setClass "messageName"
                    |> fromNoPadding messagePadding
                , Neat.text ("@" ++ screenName)
                    |> setClass "messageScreenName"
                    |> fromNoPadding messagePadding
                ]
            , Layout.column
                [ Neat.text """
                    Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                    """
                , Neat.text """
                    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
                    """
                ]
                |> setClass "messageBody"
                |> Neat.fromNoPadding messagePadding
            ]
            |> Neat.expand messagePadding bodyPadding
        ]
        |> setBoundary bodyPadding
        |> setClass "message"



-- Right Pane


rightPane : View NoPadding Msg
rightPane =
    Layout.columnWith
        { defaultColumn
            | horizontal = Column.Stretch
        }
        [ searchField
        ]
        |> setBoundary menuPadding
        |> setClass "rightPane"
        |> setLayout Layout.fill


searchField : View MenuPadding Msg
searchField =
    Layout.rowWith
        { defaultRow
            | vertical = Row.VCenter
        }
        [ Neat.empty
            |> setClass "searchIcon"
            |> fromNoPadding iconPadding
        , Neat.emptyNode Html.input
            |> setClass "searchInput"
            |> setType_ "text"
            |> setMixin
                (Mixin.fromAttribute <| Attributes.placeholder "Search Bleater")
            |> setMixin
                (Mixin.fromAttribute <| Events.onChange ChangeSearchText)
            |> fromNoPadding iconPadding
        ]
        |> setBoundary iconPadding
        |> setClass "searchField"
        |> fromNoPadding menuPadding


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "app__" ++ name


setClass : String -> View NoPadding msg -> View NoPadding msg
setClass =
    setMixin << class


setType_ : String -> View NoPadding msg -> View NoPadding msg
setType_ =
    setMixin << Mixin.fromAttribute << Attributes.type_
