module Main exposing (main)

import Gap
import Json.Decode as Decode
import Neat exposing (View, NoGap, IsGap(..), defaultRenderer)
import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events as Events
import Mixin exposing (Mixin)
import Mixin.Html as Mixin



-- App


main : Program () (Neat.Model Model) (Neat.Msg Msg)
main =
    Neat.document
        { init = \_ -> init
        , view = \model ->
            { title = "Catalog"
            , body = view model
            }
        , update = update
        , subscriptions = subscriptions
        , renderer = \_ ->
            defaultRenderer
                |> Neat.setBaseSizeInRem 1.4
        }


type alias Model =
    ()


init : ( Model, Cmd Msg )
init =
    ( ()
    , Cmd.none
    )


type Msg
    = NoOp
    | AnimationStart Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        AnimationStart n ->
            ( let _ = Debug.log "animation start" n in model
            , Cmd.none
            )


view : Model -> View NoGap Msg
view _ =
  Neat.column
    [ document "Catalog"
        [ chapter "Text block"
            [ section "Row item"
                [ Neat.textBlock "default block in default row"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                            |> Neat.setHorizontalSpaceBoth
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "fit to infinite"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMaxWidthInfinite
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setMinWidthInEm 40
                        |> Neat.setGap Gap.section
                , Neat.textBlock "fit to infinite"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMaxWidthInEm 30
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                            |> Neat.setColumn
                            |> Neat.setMaxWidthInfinite
                    ]
                        |> Neat.setMinWidthInEm 40
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 0em, wrap disabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 0
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 0em, wrap enabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 0
                            |> Neat.enableWrap
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 8em, wrap disabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 8
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 8em, wrap enabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 8
                            |> Neat.enableWrap
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "max-width: 16em, wrap enabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMaxWidthInEm 16
                            |> Neat.setAttributes
                                [ Attributes.style "animation-name" "neat-dummy-animation"
                                , Events.on "animationstart"
                                    (Decode.at [ "target", "offsetWidth" ] Decode.int
                                        |> Decode.map AnimationStart
                                    )
                                ]
                            |> Neat.enableWrap
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 16em, wrap disabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 16em, wrap enabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.enableWrap
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 12bs (= 1.4rem x 12 in this case), wrap disabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInBs 12
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 0em, min-height: 3em, wrap enabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 0
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                    |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 8em, min-height: 3em, wrap enabled
                    """
                    , Neat.textBlock """
                    max-width: 25em, max-height: 5em
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 8
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                            |> Neat.setMaxWidthInEm 25
                            |> Neat.setMaxHeightInEm 5
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 16em, min-height: 3em, wrap enabled
                    """
                    , Neat.textBlock """
                    max-width: 25em, max-height: 5em
                    """
                    , Neat.textBlock """
                    space: forward
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                            |> Neat.setMaxWidthInEm 25
                            |> Neat.setMaxHeightInEm 5
                            |> Neat.setHorizontalSpaceForward
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 16em, min-height: 3em, wrap enabled
                    """
                    , Neat.textBlock """
                    max-width: infinite, max-height: infinite
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock """
                  default block with gap
                  """
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setGap Gap.sample
                        ]
                            |> Neat.setBoundary
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 16em, min-height: 3em, wrap enabled
                    """
                    , Neat.textBlock """
                    max-width: 25em, max-height: 5em
                    """
                    , Neat.textBlock """
                    space: forward
                    """
                    , Neat.textBlock """
                    (with gap)
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                            |> Neat.setMaxWidthInEm 25
                            |> Neat.setMaxHeightInEm 5
                            |> Neat.setHorizontalSpaceForward
                            |> Neat.setGap Gap.sample
                        ]
                            |> Neat.setBoundary
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 16em, min-height: 3em, wrap enabled
                    """
                    , Neat.textBlock """
                    max-width: 25em, max-height: 5em
                    """
                    , Neat.textBlock """
                    space: forward
                    """
                    , Neat.textBlock """
                    (with gap, infinite based)
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                            |> Neat.setMaxWidthInEm 25
                            |> Neat.setMaxHeightInEm 5
                            |> Neat.setHorizontalSpaceForward
                            |> Neat.setGap Gap.sample
                        ]
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                            |> Neat.setBoundary
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                ]
            , section "Column item"
                [ Neat.textBlock "default block in default column"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 0em, wrap disabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 0
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 0em, wrap enabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 0
                            |> Neat.enableWrap
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 8em, wrap disabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 8
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 8em, wrap enabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 8
                            |> Neat.enableWrap
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 16em, wrap disabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 16em, wrap enabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.enableWrap
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 12bs (= 1.4rem x 12 in this case), wrap disabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInBs 12
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 0em, min-height: 3em, wrap enabled"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 0
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                    |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 8em, min-height: 3em, wrap enabled
                    """
                    , Neat.textBlock """
                    max-width: 25em, max-height: 5em
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 8
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                            |> Neat.setMaxWidthInEm 25
                            |> Neat.setMaxHeightInEm 5
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 16em, min-height: 3em, wrap enabled
                    """
                    , Neat.textBlock """
                    max-width: 25em, max-height: 5em
                    """
                    , Neat.textBlock """
                    space: both
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                            |> Neat.setMaxWidthInEm 25
                            |> Neat.setMaxHeightInEm 5
                            |> Neat.setVerticalSpaceBoth
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 16em, min-height: 3em, wrap enabled
                    """
                    , Neat.textBlock """
                    max-width: infinite, max-height: infinite
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock """
                  default block with gap
                  """
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setGap Gap.sample
                        ]
                            |> Neat.setBoundary
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 16em, min-height: 3em, wrap enabled
                    """
                    , Neat.textBlock """
                    max-width: 25em, max-height: 5em
                    """
                    , Neat.textBlock """
                    space: both
                    """
                    , Neat.textBlock """
                    (with gap)
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                            |> Neat.setMaxWidthInEm 25
                            |> Neat.setMaxHeightInEm 5
                            |> Neat.setVerticalSpaceBoth
                            |> Neat.setGap Gap.sample
                        ]
                            |> Neat.setBoundary
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                ]
            ]
        , chapter "Scalable block"
            [ section "Row item"
                [ Neat.textBlock "default block in default row, ratio: 0.5"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.scalableBlock 0.5
                            |> Neat.setMixin (class "withBorder1")
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 0em, ratio: 1.2"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.scalableBlock 1.2
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 0
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock "min-width: 8em, ratio: 0.5"
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.scalableBlock 0.5
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 8
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 16em, max-width: 25em, rate: 0.5
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.scalableBlock 0.5
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.setMaxWidthInEm 25
                        ]
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    min-width: 16em, max-width: 25em, ratio: 0.5
                    """
                    , Neat.textBlock """
                    (with gap)
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.column
                        [ Neat.scalableBlock 0.5
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMinWidthInEm 16
                            |> Neat.setMinHeightInEm 3
                            |> Neat.enableWrap
                            |> Neat.setMaxWidthInEm 25
                            |> Neat.setMaxHeightInEm 5
                            |> Neat.setVerticalSpaceBoth
                            |> Neat.setGap Gap.sample
                        ]
                            |> Neat.setBoundary
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section

                ]
            ]
        , chapter "Overlay"
            [ section "overlay"
                [ Neat.textBlock """
                  overlay on text block
                  """
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.textBlock shortText
                        |> Neat.setMixin (class "withBorder1")
                        |> Neat.putLayer
                            ( Neat.defaultLayer
                            , Neat.textBlock "overlay"
                                |> Neat.setMixin (class "withBorder2")
                            )
                        |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock """Expanded overlay
                  """
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.textBlock shortText
                        |> Neat.setMixin (class "withBorder1")
                        |> Neat.setMinWidthInEm 30
                        |> Neat.setMinHeightInEm 10
                        |> Neat.putLayer
                            ( Neat.defaultLayer
                            , Neat.textBlock "overlay"
                                |> Neat.setMixin (class "withBorder2")
                                |> Neat.setMaxWidthInfinite
                                |> Neat.setMaxHeightInfinite
                            )
                        |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                ]
            ]
        , chapter "Combination"
            [ section "combination"
                [ Neat.textBlock """
                  multiple blocks
                  """
                    |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setGap Gap.sample
                            |> Neat.setMinWidthInEm 0
                            |> Neat.enableWrap
                        , Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setGap Gap.sample
                            |> Neat.setMinWidthInEm 0
                        , Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setGap Gap.sample
                        ]
                            |> Neat.setMaxWidthFit
                            |> Neat.setBoundary
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.column
                    [ Neat.textBlock """
                    # Last block
                    """
                    , Neat.textBlock """
                    max-width: infinite, max-height: infinite
                    """
                    , Neat.textBlock """
                    min-width: 20em, max-height: 4em
                    """
                    ]
                        |> Neat.setMaxHeightFit
                        |> Neat.setGap Gap.section
                , sample
                    [ Neat.row
                        [ Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setGap Gap.sample
                            |> Neat.setMinWidthInEm 0
                        , Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setGap Gap.sample
                        , Neat.textBlock shortText
                            |> Neat.setMixin (class "withBorder1")
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                            |> Neat.setMinWidthInEm 20
                            |> Neat.setMinHeightInEm 4
                            |> Neat.setGap Gap.sample
                        ]
                            |> Neat.setBoundary
                            |> Neat.setMaxWidthInfinite
                            |> Neat.setMaxHeightInfinite
                            |> Neat.setMixin (class "withBorder2")
                            |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock """
                  sample row 1
                  """
                    |> Neat.setGap Gap.section
                , sample
                    [ sampleRow1
                        |> Neat.setBoundary
                        |> Neat.setMaxWidthInfinite
                        |> Neat.setMixin (class "withBorder2")
                        |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock """
                  sample row 2
                  """
                    |> Neat.setGap Gap.section
                , sample
                    [ sampleRow2
                        |> Neat.setBoundary
                        |> Neat.setMaxWidthInfinite
                        |> Neat.setMixin (class "withBorder2")
                        |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock """
                  sample column 1
                  """
                    |> Neat.setGap Gap.section
                , sample
                    [ sampleColumn1
                        |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                , Neat.textBlock """
                  sample column wrapper
                  """
                    |> Neat.setGap Gap.section
                , sample
                    [ sampleColumnWrapper
                        |> Neat.setBoundary
                        |> Neat.setMaxWidthInfinite
                        |> Neat.setMixin (class "withBorder2")
                        |> Neat.setGap Gap.sample
                    ]
                        |> Neat.setGap Gap.section
                ]
            ]
        ]
            |> Neat.setBoundary
            |> Neat.setMaxWidthInfinite
            |> Neat.setMaxHeightInfinite
            |> Neat.setLineHeight 2.5
    ]


sampleRow1 =
    Neat.row
        [ Neat.textBlock shortText
            |> Neat.setMaxWidthInEm 16
            |> Neat.setMinWidthInEm 8
            |> Neat.setMaxHeightInEm 5
            |> Neat.setMinHeightInEm 3
            |> Neat.enableWrap
            |> Neat.setMixin (class "withBorder1")
            |> Neat.setGap Gap.sample
        , Neat.textBlock shortText
            |> Neat.setMixin (class "withBorder1")
            |> Neat.setGap Gap.sample
        ]
            |> Neat.setHorizontalSpaceForward

sampleRow2 =
    Neat.row
        [ Neat.scalableBlock 0.25
            |> Neat.setMaxWidthInEm 16
            |> Neat.setMinWidthInEm 8
            |> Neat.setMixin (class "withBorder1")
            |> Neat.setGap Gap.sample
        ]
            |> Neat.setHorizontalSpaceForward


sampleColumn1 =
    Neat.column
        [ sampleRow1
            |> Neat.setMaxHeightFit
            |> Neat.setBoundary
            |> Neat.setMaxWidthInfinite
            |> Neat.setMaxHeightInfinite
            |> Neat.setMixin (class "withBorder2")
            |> Neat.setGap Gap.sample
        , sampleRow2
            |> Neat.setBoundary
            |> Neat.setMaxWidthInfinite
            |> Neat.setMixin (class "withBorder2")
            |> Neat.setGap Gap.sample
        ]


sampleColumnWrapper =
    Neat.column
        [ sampleColumn1
            |> Neat.setGap Gap.sample
        ]


document : String -> List (View Gap.Chapter msg) -> View Gap.Document msg
document title children =
    Neat.column
        [ documentTitle title
        , children
            |> Neat.column
            |> Neat.setGap Gap.document
        ]

documentTitle : String -> View Gap.Document msg
documentTitle title =
    Neat.column
        [ Neat.textBlock title
            |> Neat.setNodeName "h1"
            |> Neat.setMixin (class "document_title")
        , Neat.empty
            |> Neat.setMinHeightInEm 0.3
            |> Neat.setMixin (class "document_title_border")
        ]
            |> Neat.setMaxWidthFit
            |> Neat.setBoundary
            |> Neat.setGap Gap.document
            |> Neat.setMaxWidthInfinite
            |> Neat.setVerticalSpaceBoth

chapter : String -> List (View Gap.Section msg) -> View Gap.Chapter msg
chapter title children =
    Neat.column
        [ chapterTitle title
        , Neat.column children
            |> Neat.setBoundary
            |> Neat.setMaxWidthInfinite
            |> Neat.setMaxHeightInfinite
            |> Neat.setGap Gap.chapter
        ]


chapterTitle : String -> View Gap.Chapter msg
chapterTitle title =
    Neat.row
        [ Neat.textBlock title
            |> Neat.setNodeName "h2"
            |> Neat.setMixin (class "chapter_title")
            |> Neat.setMixin (Mixin.id title)
        ]
            |> Neat.setNodeName "a"
            |> Neat.setAttribute (Attributes.href <| "#" ++ title)
        |> Neat.setGap Gap.chapter

section : String -> List (View Gap.Section msg) -> View Gap.Section msg
section title children =
    sectionTitle title :: children
        |>  Neat.column


sectionTitle : String -> View Gap.Section msg
sectionTitle title =
    Neat.textBlock title
        |> Neat.setNodeName "h3"
        |> Neat.setMixin (class "section_title")
        |> Neat.setGap Gap.section
        |> Neat.setMaxWidthInfinite


sample : List (View Gap.Sample msg) -> View NoGap msg
sample children =
    Neat.row children
        |> Neat.setMaxWidthInfinite
        |> Neat.setMaxHeightInfinite
        |> Neat.setBoundary
        |> Neat.setMixin (class "sample")
        |> Neat.setMaxWidthInfinite
        |> Neat.setMaxHeightInfinite
        |> Neat.setMaxWidthInEm 50


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

longText : String
longText = """Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
"""

mediumText : String
mediumText = """Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
"""

shortText : String
shortText = """Lorem ipsum dolor sit amet
"""



-- Helper functions


{-| A specialized version of `class` for this module.
It converts given class names into ones generated by CSS modules.
-}
class : String -> Mixin msg
class =
    classMixinWith <| \name -> "app__" ++ name
