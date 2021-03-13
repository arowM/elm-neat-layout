module Gap exposing
    ( Chapter
    , Document
    , Sample
    , Section
    , chapter
    , document
    , sample
    , section
    )

import Neat exposing (IsGap(..))


type Document
    = Document


document : IsGap Document
document =
    IsGap
        { horizontal = 1.4
        , vertical = 1.4
        }


type Chapter
    = Chapter


chapter : IsGap Chapter
chapter =
    IsGap
        { horizontal = 0.6
        , vertical = 0.6
        }


type Section
    = Section


section : IsGap Section
section =
    IsGap
        { horizontal = 0.6
        , vertical = 0.6
        }


type Sample
    = Sample


sample : IsGap Sample
sample =
    IsGap
        { horizontal = 0.3
        , vertical = 0.3
        }
