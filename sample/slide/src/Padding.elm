module Padding exposing
    ( PagePadding
    , SubPadding
    , pagePadding
    , subPadding
    )

import Neat exposing (IsPadding(..))


type PagePadding
    = PagePadding


pagePadding : IsPadding PagePadding
pagePadding =
    IsPadding
        { rem = 1.4
        }


type SubPadding
    = SubPadding


subPadding : IsPadding SubPadding
subPadding =
    IsPadding
        { rem = 0.6
        }
