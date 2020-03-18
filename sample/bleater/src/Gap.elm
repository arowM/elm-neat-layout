module Gap exposing
    ( BodyGap
    , IconGap
    , MenuGap
    , MessageGap
    , HeaderGap
    , bodyGap
    , iconGap
    , menuGap
    , messageGap
    , headerGap
    )

import Neat exposing (IsGap(..))


type MenuGap
    = MenuGap


menuGap : IsGap MenuGap
menuGap =
    IsGap
        { rem = menuRem
        }


-- Gap for texts with icon.


type IconGap
    = IconGap


iconGap : IsGap MenuGap
iconGap =
    IsGap
        { rem = iconRem
        }



-- Gap for body.


type BodyGap
    = BodyGap


bodyGap : IsGap BodyGap
bodyGap =
    IsGap
        { rem = 0.6
        }


-- Gap for message


type MessageGap
    = MessageGap


messageGap : IsGap MessageGap
messageGap =
    IsGap
        { rem = 0.4
        }


-- Gap for headers


type HeaderGap
    = HeaderGap


headerGap : IsGap HeaderGap
headerGap =
    IsGap
        { rem = menuRem + iconRem
        }


-- Constant values


menuRem : Float
menuRem = 0.8

iconRem : Float
iconRem = 0.6
