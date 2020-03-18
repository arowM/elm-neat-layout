module Gap exposing
    ( PageGap
    , SubGap
    , pageGap
    , subGap
    )

import Neat exposing (IsGap(..))


type PageGap
    = PageGap


pageGap : IsGap PageGap
pageGap =
    IsGap
        { rem = 1.4
        }


type SubGap
    = SubGap


subGap : IsGap SubGap
subGap =
    IsGap
        { rem = 0.6
        }
