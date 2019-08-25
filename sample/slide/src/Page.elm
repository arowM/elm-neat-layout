module Page exposing (Page, pages)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Lazy as Html
import Markdown
import Mixin exposing (Mixin)
import Neat exposing (NoPadding, Protected, View, fromNoPadding, setBoundary, setLayout, setMixin)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (Column, defaultColumn)
import Neat.Layout.Row as Row exposing (defaultRow)
import Padding exposing (PagePadding, SubPadding, pagePadding, subPadding)


type alias Page msg =
    View NoPadding msg


pages : List (Page msg)
pages =
    [ template Nothing titlePage
    , template (Just "自己紹介") self
    , template (Just "概要") abstPage
    , template (Just "に〜と とは") aboutNeatPage
    , template (Just "に〜と には余白が必要") paddingAndNeatPage
    , template (Just "に〜と でいることの難しさ") difficultyOfNeat
    , template (Just "elm-neat-layoutなら に〜と！") neatPadding
    , template (Just "elm-neat-layoutなら に〜と！") neatPadding2
    , template (Just "型安全に〜と！") typesafePadding
    , template (Just "概要ふたたび ネコにまたたび") abstPage2
    , template (Just "レイアウト専用とは") aboutLayout
    , template (Just "Row") aboutRow
    , template (Just "Column") aboutColumn
    , template (Just "その他の便利機能") others
    , template (Just "おわり") last
    ]


{-| Return current page view.
-}
template : Maybe String -> View PagePadding msg -> Page msg
template mtitle c =
    Layout.columnWith
        { defaultColumn
            | horizontal = Column.HCenter
        }
        [ titleBoundary mtitle
        , c
            |> setBoundary pagePadding
            |> setClass "contentBoundary"
            |> setLayout Layout.fill
        ]
        |> setClass "page"


titleBoundary : Maybe String -> View NoPadding msg
titleBoundary mtitle =
    case mtitle of
        Nothing ->
            Neat.none

        Just title ->
            Neat.text title
                |> setClass "title"
                |> fromNoPadding pagePadding
                |> setBoundary pagePadding
                |> setClass "titleBoundary"
                |> setLayout Layout.noShrink



-- Content


titlePage : View PagePadding msg
titlePage =
    Layout.columnWith
        { defaultColumn
            | horizontal = Column.Stretch
        }
        [ Neat.text "に〜と のためのキマるスタイリング🐐"
            |> centerize
            |> fromNoPadding pagePadding
            |> setLayout Layout.fill
        , Neat.div []
            [ Layout.columnWith
                { defaultColumn
                    | horizontal = Column.Right
                }
                [ Neat.text "2019/8/25 Elm Meetup in Summer"
                    |> setClass "subText"
                    |> fromNoPadding subPadding
                , Neat.lift Html.a
                    [ Mixin.fromAttribute <| Attributes.href "https://twitter.com/arowM_"
                    , Mixin.fromAttribute <| Attributes.target "_blank"
                    , Mixin.fromAttribute <| Attributes.rel "noopener"
                    ]
                    [ Neat.text "arowM_"
                    ]
                    |> setClass "subText"
                    |> fromNoPadding subPadding
                ]
                |> Neat.expand subPadding pagePadding
            ]
        ]


self : View PagePadding msg
self =
    md """
## arowM_

ヤギの娘さくらちゃんと暮らす未婚の父

<div class="page__sakura"></div>

フリーランスUXハッカー

* ARoW (システム開発) 代表
* 気吹堂 (出版) 代表
    * さくらちゃんの写真集がでるよ
* 株式会社UZUZ (人材紹介) CXO (Chief eXperience Officer)
    """


abstPage : View PagePadding msg
abstPage =
    md """
さっきリリースした elm-neat-layout の紹介

* **に〜と** にビューを組み立てられる
* レイアウト専用ライブラリ

このスライドも elm-neat-layout で作ってるよ！
    """


aboutNeatPage : View PagePadding msg
aboutNeatPage =
    md """
## に〜と == Neat

きちんとした、こぎれいな、こざっぱりした、きれい好きな、身だしなみのよい、整った(形をした)、均整のとれた、手際のいい、巧妙な、適切な

(by Weblio)
    """


paddingAndNeatPage : View PagePadding msg
paddingAndNeatPage =
    md """
## 余白の性質

**不均一な余白は画面に動きを作り出す**

* 絵画の場合:
    * 不均一でないと死んだ絵になる
* ウェブアプリの場合:
    * 不均一だと情報過多になって伝えたい情報が伝わらない

→ ウェブアプリには **に〜とな余白** が重要
"""


difficultyOfNeat : View PagePadding msg
difficultyOfNeat =
    md """
elm-html や elm-ui では に〜とな余白がむずかしい

複数のビューを組み立てる時に破綻する

<div class="page__row-space-between">
    <div class="page__padding1"><div class="page__view1"></div></div>
    <div class="page__padding1"><div class="page__view2"></div></div>
</div>

この2つを結合すると...

<div class="page__row-center">
    <div class="page__padding1"><div class="page__view1"></div></div><div class="page__padding1"><div class="page__view2"></div></div>
</div>

真ん中だけ余白が2倍になってしまう😩
    """


neatPadding : View PagePadding msg
neatPadding =
    md """
```elm
view1 : View MyPadding msg
view1 =
    Neat.fromNoPadding myPadding <|
        Neat.div
            [ class "view1"
            ]
            [ text "View 1"
            ]

view2 : View MyPadding msg
view2 =
    Neat.fromNoPadding myPadding <|
        Neat.lift Html.div
            [ class "view2"
            ]
            [ text "View 2"
            ]
```

`Neat.div` でも `Html.lift Html.div` でもどっちでもOK！
    """


neatPadding2 : View PagePadding msg
neatPadding2 =
    md """
```elm
composed : View MyPadding msg
composed =
    Layout.row
        [ view1
        , view2
        ]
```

<div class="page__row-space-between">
    <div class="page__padding1"><div class="page__view1"></div></div>
    <div class="page__padding1"><div class="page__view2"></div></div>
</div>

この2つが...

<div class="page__row-center">
<div class="page__padding1 page__row-center page__viewWrapper"><div class="page__view1"></div><div class="page__space1"></div><div class="page__view2"></div>
</div>

うまくくっついた！
    """


typesafePadding : View PagePadding msg
typesafePadding =
    md """
自分専用の余白を定義したり

```elm
type MyPadding
    = MyPadding

myPadding : IsPadding MyPadding
myPadding =
    IsPadding
        { rem = 1.4
        }
```

余白を設定したり

```elm
fromNoPadding : IsPadding p
             -> View NoPadding msg
             -> View p msg
```

例:

```elm
view0 : View NoPadding msg
view0 =
    Neat.text "view0"

view1 : View MyPadding msg
view1 =
    fromNoPadding myPadding view0
```

余白を消したり

```elm
setBoundary : IsPadding p
           -> View p msg
           -> View NoPadding msg
```

余白を増やしたり

```elm
expand : IsPadding p1
      -> IsPadding p2
      -> View p1 msg
      -> View p2 msg
```

に〜とじゃないとコンパイルが通らないよ！
    """


abstPage2 : View PagePadding msg
abstPage2 =
    md """
elm-neat-layout の特徴

* **に〜と** にビューを組み立てられる
* レイアウト専用ライブラリ
**↑↑ 今度はこっちの話 ↑↑**
    """


aboutLayout : View PagePadding msg
aboutLayout =
    md """
elm-neat-layout は、CSSを置き換えるもの **ではない**

* 実は、CSSの嫌なところって **レイアウト** じゃない？
    * 横並びとか、中央配置とか
* `background-color` とか別にCSSで書いても苦痛じゃないよね？
* ウェブ自体はCSSだから、elm-ui使っても結局CSSは必要だよね？

→ elm-neat-layout の方針:
\u{3000}変にCSSを置き換えようとしないで
\u{3000}レイアウトだけElmで書く
    """


aboutRow : View PagePadding msg
aboutRow =
    md """
横並び:
```elm
view1 : View MyPadding msg
view1 = Debug.todo "省略"

view2 : View MyPadding msg
view2 = Debug.todo "省略"

Layout.row
    [ view1
    , view2
    ]
```
余白をいい感じにしてくれるよ！


細かい指定:
```elm
Layout.rowWith
    { defaultRow
      | wrap = True
      , horizontal = Row.Right
      , vertical = Row.Bottom
    }
    [ view1
      |> Neat.setLayout Layout.noShrink
    , view2
      |> Neat.setLayout Layout.fill
    ]
```

中央配置
```elm
Layout.rowWith
    { defaultRow
      | vertical = Row.VCenter
      , horizontal = Row.HCenter
    }
    [ view1
    , view2
    ]
```
    """


aboutColumn : View PagePadding msg
aboutColumn =
    md """
縦並び:
```elm
view1 : View MyPadding msg
view1 = Debug.todo "省略"

view2 : View MyPadding msg
view2 = Debug.todo "省略"

Layout.column
    [ view1
    , view2
    ]
```
余白をいい感じにしてくれるよ！


細かい指定:
```elm
Layout.columnWith
    { defaultRow
      | horizontal = Column.HCenter
      , vertical = Column.Stretch
    }
    [ view1
      |> Neat.setLayout Layout.noShrink
      |> Neat.setLayout Layout.fillBy 2
    , view2
      |> Neat.setLayout Layout.fillBy 1
    ]
```
    """


others : View PagePadding msg
others =
    md """
パイプラインスタイルで書きやすい！

```elm
Layout.columnWith
    { defaultColumn
        | horizontal = Column.HCenter
    }
    [ titleBoundary mtitle
    , content
        |> setBoundary pagePadding
        |> setClass "contentBoundary"
        |> setMixin
            ( if isSpecial then
                Mixin.class "special"
              else
                Mixin.none
            )
        |> setLayout Layout.fill
    ]
    |> setClass "page"
```
    """


last : View PagePadding msg
last =
    md """
もっと詳しく知りたくなったら、

* [パッケージ](https://package.elm-lang.org/packages/arowM/elm-neat-layout/latest/)
* [GitHubリポジトリのサンプル](https://github.com/arowM/elm-neat-layout/tree/slide/sample)
    """



-- Helper View functions


centerize : View NoPadding msg -> View NoPadding msg
centerize v =
    Layout.rowWith
        { defaultRow
            | horizontal = Row.HCenter
            , vertical = Row.VCenter
        }
        [ v ]


md : String -> View PagePadding msg
md =
    Neat.unsafeFromHtml << Html.lazy markdown_


markdown_ : String -> Html msg
markdown_ =
    Markdown.toHtmlWith
        { githubFlavored = Just { tables = True, breaks = True }
        , defaultHighlighting = Nothing
        , sanitize = False
        , smartypants = False
        }
        (Mixin.toAttributes <| class "markdown")



-- Helper function


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "page__" ++ name


setClass : String -> View NoPadding msg -> View NoPadding msg
setClass =
    setMixin << class
