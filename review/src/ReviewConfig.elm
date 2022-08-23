module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule as Rule exposing (Rule)
import NoAlways
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Simplify


config : List Rule
config =
    [ NoAlways.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/Neat.elm"
            , "src/Neat/View.elm"
            , "src/Neat/Boundary.elm"
            ]
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.CustomTypeConstructors.rule
        []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForDirectories
            [ "tests/VerifyExamples"
            , "src/Neat"
            , "sample/app"
            ]
        |> Rule.ignoreErrorsForFiles
            [ "src/Neat.elm"
            ]
    , NoUnused.Modules.rule
        |> Rule.ignoreErrorsForDirectories
            [ "src/Neat"
            , "sample/app"
            ]
        |> Rule.ignoreErrorsForFiles
            [ "src/Neat.elm"
            ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.defaults
        |> Simplify.rule
    ]
        |> List.map
            ( Rule.ignoreErrorsForDirectories
                [ "tests/VerifyExamples"
                ]
            )
