module Gap exposing
    ( Body
    , Header
    , Icon
    , Menu
    , Message
    , body
    , header
    , icon
    , menu
    , message
    )

import Neat exposing (IsGap(..))


type Menu
    = Menu


menu : IsGap Menu
menu =
    IsGap
        { width = menuRem
        , height = menuRem
        }



-- Gap for texts with icon.


type Icon
    = Icon


icon : IsGap Icon
icon =
    IsGap
        { width = iconRem
        , height = iconRem
        }



-- Gap for body.


type Body
    = Body


body : IsGap Body
body =
    IsGap
        { width = 0.6
        , height = 0.6
        }



-- Gap for message


type Message
    = Message


message : IsGap Message
message =
    IsGap
        { width = 0.4
        , height = 0.4
        }



-- Gap for headers


type Header
    = Header


header : IsGap Header
header =
    IsGap
        { width = menuRem + iconRem
        , height = menuRem + iconRem
        }



-- Constant values


menuRem : Float
menuRem =
    0.8


iconRem : Float
iconRem =
    0.6
