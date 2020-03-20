module Gap exposing
    ( Page
    , Sub
    , page
    , sub
    )

import Neat exposing (IsGap(..))


type Page
    = Page


page: IsGap Page
page =
    IsGap
        { height = 1.4
        , width = 1.4
        }


type Sub
    = Sub


sub: IsGap Sub
sub=
    IsGap
        { height = 0.6
        , width = 0.6
        }
