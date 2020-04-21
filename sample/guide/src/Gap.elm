module Gap exposing
    ( CodeLine
    , codeLine
    , Editor
    , editor
    , Preview
    , preview
    , Repl
    , repl
    )

{-| Declare gaps

@docs CodeLine
@docs codeLine
@docs Editor
@docs editor
@docs Preview
@docs preview
@docs Repl
@docs repl

-}

import Neat exposing (IsGap(..))


{-| -}
type CodeLine
    = CodeLine


{-| -}
codeLine : IsGap CodeLine
codeLine =
    IsGap
        { width = 0.4
        , height = 0.4
        }


{-| Gaps for Editors
-}
type Editor = Editor


{-| -}
editor : IsGap Editor
editor =
    IsGap
        { width = 0.4
        , height = 0.4
        }


{-| Gaps for Previewers
-}
type Preview = Preview


{-| -}
preview : IsGap Preview
preview =
    IsGap
        { width = 0.4
        , height = 0.4
        }

{-| Gaps for repl windows
-}
type Repl = Repl


{-| -}
repl =
    IsGap
        { width = 0.4
        , height = 0.4
        }
