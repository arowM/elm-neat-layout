module Padding exposing
    ( BodyPadding
    , IconPadding
    , MenuPadding
    , MessagePadding
    , HeaderPadding
    , bodyPadding
    , iconPadding
    , menuPadding
    , messagePadding
    , headerPadding
    )

import Neat exposing (IsPadding(..))


type MenuPadding
    = MenuPadding


menuPadding : IsPadding MenuPadding
menuPadding =
    IsPadding
        { rem = menuRem
        }


-- Padding for texts with icon.


type IconPadding
    = IconPadding


iconPadding : IsPadding MenuPadding
iconPadding =
    IsPadding
        { rem = iconRem
        }



-- Padding for body.


type BodyPadding
    = BodyPadding


bodyPadding : IsPadding BodyPadding
bodyPadding =
    IsPadding
        { rem = 0.6
        }


-- Padding for message


type MessagePadding
    = MessagePadding


messagePadding : IsPadding MessagePadding
messagePadding =
    IsPadding
        { rem = 0.4
        }


-- Padding for headers


type HeaderPadding
    = HeaderPadding


headerPadding : IsPadding HeaderPadding
headerPadding =
    IsPadding
        { rem = menuRem + iconRem
        }


-- Constant values


menuRem : Float
menuRem = 0.8

iconRem : Float
iconRem = 0.6
