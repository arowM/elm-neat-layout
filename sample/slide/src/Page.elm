module Page exposing (Page, pages)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Lazy as Html
import Markdown
import Mixin exposing (Mixin)
import Neat exposing (NoGap, View, fromNoGap, setBoundary, setLayout, setMixin)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (defaultColumn)
import Neat.Layout.Row as Row exposing (defaultRow)
import Gap exposing (PageGap, pageGap, subGap)


type alias Page msg =
    View NoGap msg


pages : List (Page msg)
pages =
    [ template Nothing titlePage
    , template (Just "自己紹介") self
    , template (Just "概要") abstPage
    , template (Just "に〜と とは") aboutNeatPage
    , template (Just "に〜と には余白が必要") gapAndNeatPage
    , template (Just "に〜と でいることの難しさ") difficultyOfNeat
    , template (Just "elm-neat-layoutなら に〜と！") neatGap
    , template (Just "elm-neat-layoutなら に〜と！") neatGap2
    , template (Just "型安全に〜と！") typesafeGap
    , template (Just "概要ふたたび ネコにまたたび") abstPage2
    , template (Just "レイアウト専用とは") aboutLayout
    , template (Just "Row") aboutRow
    , template (Just "Column") aboutColumn
    , template (Just "その他の便利機能") others
    , template (Just "おわり") last
    ]


{-| Return current page view.
-}
template : Maybe String -> View PageGap msg -> Page msg
template mtitle c =
    Layout.columnWith
        { defaultColumn
            | horizontal = Column.HCenter
        }
        [ titleBoundary mtitle
        , c
            |> setBoundary pageGap
            |> setClass "contentBoundary"
            |> setLayout Layout.fill
        ]
        |> setClass "page"


titleBoundary : Maybe String -> View NoGap msg
titleBoundary mtitle =
    case mtitle of
        Nothing ->
            Neat.none

        Just title ->
            Neat.textBlock title
                |> setClass "title"
                |> fromNoGap pageGap
                |> setBoundary pageGap
                |> setClass "titleBoundary"
                |> setLayout Layout.noShrink



-- Content


titlePage : View PageGap msg
titlePage =
    Layout.columnWith
        { defaultColumn
            | horizontal = Column.Stretch
        }
        [ Neat.textBlock "に〜と のためのキマるスタイリング🐐"
            |> centerize
            |> fromNoGap pageGap
            |> setLayout Layout.fill
        , Layout.row
            [ Layout.columnWith
                { defaultColumn
                    | horizontal = Column.Right
                }
                [ Neat.textBlock "2019/8/25 Elm Meetup in Summer"
                    |> setClass "subText"
                    |> fromNoGap subGap
                , Neat.lift Html.a
                    [ Mixin.fromAttribute <| Attributes.href "https://twitter.com/arowM_"
                    , Mixin.fromAttribute <| Attributes.target "_blank"
                    , Mixin.fromAttribute <| Attributes.rel "noopener"
                    ]
                    [ Neat.textBlock "arowM_"
                    ]
                    |> setClass "subText"
                    |> fromNoGap subGap
                ]
                |> Neat.expand subGap pageGap
            ]
        ]


self : View PageGap msg
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

最近つくった elm-form-decoder もよろしくね！
    """


abstPage : View PageGap msg
abstPage =
    md """
さっきリリースした elm-neat-layout の紹介

* **に〜と** にビューを組み立てられる
* レイアウト専用ライブラリ

このスライドも elm-neat-layout で作ってるよ！
    """


aboutNeatPage : View PageGap msg
aboutNeatPage =
    md """
## に〜と == Neat

きちんとした、こぎれいな、こざっぱりした、きれい好きな、身だしなみのよい、整った(形をした)、均整のとれた、手際のいい、巧妙な、適切な

(by Weblio)
    """


gapAndNeatPage : View PageGap msg
gapAndNeatPage =
    md """
## 余白の性質

**不均一な余白は画面に動きを作り出す**

* 絵画の場合:
    * 不均一でないと死んだ絵になる
* ウェブアプリの場合:
    * 不均一だと情報過多になって伝えたい情報が伝わらない

→ ウェブアプリには **に〜とな余白** が重要
"""


difficultyOfNeat : View PageGap msg
difficultyOfNeat =
    md """
elm-html や elm-ui では に〜とな余白がむずかしい

複数のビューを組み立てる時に破綻する

<div class="page__row-space-between">
    <div class="page__gap1"><div class="page__view1"></div></div>
    <div class="page__gap1"><div class="page__view2"></div></div>
</div>

この2つを結合すると...

<div class="page__row-center">
    <div class="page__gap1"><div class="page__view1"></div></div><div class="page__gap1"><div class="page__view2"></div></div>
</div>

真ん中だけ余白が2倍になってしまう😩
    """


neatGap : View PageGap msg
neatGap =
    md """
```elm
view1 : View MyGap msg
view1 =
    Neat.fromNoGap myGap <|
        Neat.div
            [ class "view1"
            ]
            [ textBlock "View 1"
            ]

view2 : View MyGap msg
view2 =
    Neat.fromNoGap myGap <|
        Neat.lift Html.div
            [ class "view2"
            ]
            [ textBlock "View 2"
            ]
```

`Neat.div` でも `Html.lift Html.div` でもどっちでもOK！
    """


neatGap2 : View PageGap msg
neatGap2 =
    md """
```elm
composed : View MyGap msg
composed =
    Layout.row
        [ view1
        , view2
        ]
```

<div class="page__row-space-between">
    <div class="page__gap1"><div class="page__view1"></div></div>
    <div class="page__gap1"><div class="page__view2"></div></div>
</div>

この2つが...

<div class="page__row-center">
<div class="page__gap1 page__row-center page__viewWrapper"><div class="page__view1"></div><div class="page__space1"></div><div class="page__view2"></div>
</div>

うまくくっついた！
    """


typesafeGap : View PageGap msg
typesafeGap =
    md """
自分専用の余白を定義したり

```elm
type MyGap
    = MyGap

myGap : IsGap MyGap
myGap =
    IsGap
        { rem = 1.4
        }
```

余白を設定したり

```elm
fromNoGap : IsGap p
             -> View NoGap msg
             -> View p msg
```

例:

```elm
view0 : View NoGap msg
view0 =
    Neat.textBlock "view0"

view1 : View MyGap msg
view1 =
    fromNoGap myGap view0
```

余白を消したり

```elm
setBoundary : IsGap p
           -> View p msg
           -> View NoGap msg
```

余白を増やしたり

```elm
expand : IsGap p1
      -> IsGap p2
      -> View p1 msg
      -> View p2 msg
```

に〜とじゃないとコンパイルが通らないよ！
    """


abstPage2 : View PageGap msg
abstPage2 =
    md """
elm-neat-layout の特徴

* **に〜と** にビューを組み立てられる
* レイアウト専用ライブラリ
**↑↑ 今度はこっちの話 ↑↑**
    """


aboutLayout : View PageGap msg
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


aboutRow : View PageGap msg
aboutRow =
    md """
横並び:
```elm
view1 : View MyGap msg
view1 = Debug.todo "省略"

view2 : View MyGap msg
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


aboutColumn : View PageGap msg
aboutColumn =
    md """
縦並び:
```elm
view1 : View MyGap msg
view1 = Debug.todo "省略"

view2 : View MyGap msg
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


others : View PageGap msg
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
        |> setBoundary pageGap
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


last : View PageGap msg
last =
    md """
もっと詳しく知りたくなったら、

* [パッケージ](https://package.elm-lang.org/packages/arowM/elm-neat-layout/latest/)
* [GitHubリポジトリのサンプル](https://github.com/arowM/elm-neat-layout/tree/master/sample)
    """



-- Helper View functions


centerize : View NoGap msg -> View NoGap msg
centerize v =
    Layout.rowWith
        { defaultRow
            | horizontal = Row.HCenter
            , vertical = Row.VCenter
        }
        [ v ]


md : String -> View PageGap msg
md str =
    Neat.lift
        (\attrs _ -> Html.lazy2 markdown_ attrs str)
        [ class "markdown"
        ]
        []
        |> fromNoGap pageGap


markdown_ : List (Attribute msg) -> String -> Html msg
markdown_ =
    Markdown.toHtmlWith
        { githubFlavored = Just { tables = True, breaks = True }
        , defaultHighlighting = Nothing
        , sanitize = False
        , smartypants = False
        }



-- Helper function


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "page__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class
