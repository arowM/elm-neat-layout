module Repl.GapEditor.CreateForm exposing (Error, CreateForm, decoder, init)

import Form.Decoder as FD exposing (Decoder)
import Repl.Gap exposing (Gap)


type alias CreateForm =
    { name : String
    , width : String
    , height : String
    }


init : CreateForm
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


decoder : Decoder CreateForm Error Gap
decoder =
    FD.map3
        Gap
        (FD.lift .name nameDecoder)
        (FD.lift .width widthDecoder)
        (FD.lift .height heightDecoder)
