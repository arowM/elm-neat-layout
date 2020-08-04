module Repl.GapEditor.GapForm exposing (Error, GapForm, decoder, init)

import Form.Decoder as FD exposing (Decoder)
import Repl.Ast.Gap exposing (GapSize)


type alias GapForm =
    { name : String
    , width : String
    , height : String
    }


init : GapForm
init =
    { name = ""
    , width = ""
    , height = ""
    }


type Error
    = EmptyName
    | NotAFloatWidth
    | NotAFloatHeight


nameDecoder : Decoder String Error String
nameDecoder =
    FD.identity
        |> FD.assert (FD.minLength EmptyName 1)


widthDecoder : Decoder String Error Float
widthDecoder =
    FD.float NotAFloatWidth


heightDecoder : Decoder String Error Float
heightDecoder =
    FD.float NotAFloatHeight


decoder : Decoder GapForm Error ( String, GapSize )
decoder =
    FD.map3
        (\name width height ->
            ( name
            , { width = width
              , height = height
              }
            )
        )
        (FD.lift .name nameDecoder)
        (FD.lift .width widthDecoder)
        (FD.lift .height heightDecoder)
