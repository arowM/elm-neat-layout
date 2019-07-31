module Main exposing (main)

import Atom.Input as Input
import Atom.Select as Select
import Browser
import Browser.Navigation
import Css
import Form.Decoder as Decoder
import Goat exposing (Goat)
import Goat.Age
import Goat.ContactType as ContactType
import Goat.Email
import Goat.Horns
import Goat.Message
import Goat.Name
import Goat.Phone
import Html exposing (Html, button, div, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Layout.Mixin as Mixin
import Mixin exposing (Mixin)
import Mixin.Extra as Mixin
import Neat exposing (View)
import Neat.Alignment as Neat
import Neat.Alignment.Row as Row exposing (defaultRow)
import Neat.Padding.FullPadding as FullPadding exposing (FullPadding)
import Neat.Padding.MiddlePadding as MiddlePadding
import Neat.Padding.NoPadding as NoPadding exposing (Atom)



-- App


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { registerForm : Goat.RegisterForm
    , goats : List Goat
    , pageState : PageState
    }


type PageState
    = Registering
    | FixingRegisterErrors
    | ShowGoats


init : ( Model, Cmd Msg )
init =
    ( { registerForm = Goat.init
      , goats = []
      , pageState = Registering
      }
    , Cmd.none
    )


type
    Msg
    -- Register Form
    = ChangeName String
    | ChangeAge String
    | ChangeHorns String
    | ChangeEmail String
    | ChangePhone String
    | ChangeMessage String
    | ChangeContactType String
    | SubmitRegister
    | RegisterAnotherGoat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ registerForm } as model) =
    let
        setRegisterForm : Goat.RegisterForm -> Model
        setRegisterForm a =
            { model | registerForm = a }
    in
    case msg of
        ChangeName name ->
            ( setRegisterForm
                { registerForm
                    | name = Input.fromString name
                }
            , Cmd.none
            )

        ChangeAge age ->
            ( setRegisterForm
                { registerForm
                    | age = Input.fromString age
                }
            , Cmd.none
            )

        ChangeHorns horns ->
            ( setRegisterForm
                { registerForm
                    | horns = Input.fromString horns
                }
            , Cmd.none
            )

        ChangeEmail email ->
            ( setRegisterForm
                { registerForm
                    | email = Input.fromString email
                }
            , Cmd.none
            )

        ChangePhone phone ->
            ( setRegisterForm
                { registerForm
                    | phone = Input.fromString phone
                }
            , Cmd.none
            )

        ChangeMessage message ->
            ( setRegisterForm
                { registerForm
                    | message = Input.fromString message
                }
            , Cmd.none
            )

        ChangeContactType ctype ->
            ( setRegisterForm
                { registerForm
                    | contactType = Select.fromString ctype
                }
            , Cmd.none
            )

        SubmitRegister ->
            onSubmitRegister model

        RegisterAnotherGoat ->
            ( { model
                | pageState = Registering
              }
            , Cmd.none
            )


onSubmitRegister : Model -> ( Model, Cmd Msg )
onSubmitRegister model =
    case Decoder.run Goat.decoder model.registerForm of
        Ok g ->
            ( { model
                | pageState = ShowGoats
                , goats = g :: model.goats
                , registerForm = Goat.init
              }
            , Browser.Navigation.load "#"
            )

        Err _ ->
            ( { model
                | pageState = FixingRegisterErrors
              }
            , Browser.Navigation.load "#goat-registerForm"
            )


view : Model -> Html Msg
view model =
    Neat.toPage <|
        Neat.div
            [ class "wrapper" ]
            [ background
            , Neat.setBoundary
                [ class "body"
                ]
                [ Neat.rowWith
                    { defaultRow
                        | horizontal = Row.hcenter
                    }
                    [ Neat.div
                        [ class "body_inner"
                        ]
                        [ case model.pageState of
                            ShowGoats ->
                                goats_view model.goats

                            Registering ->
                                registerForm_view False model

                            FixingRegisterErrors ->
                                registerForm_view True model
                        ]
                    ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Helper views


background : Atom Msg
background =
    Neat.div
        [ class "background" ]
        [ Neat.div
            [ class "background_header"
            ]
            []
        ]



-- Goats


goats_view : List Goat -> View FullPadding Msg
goats_view goats =
    Neat.batch
        [ Goat.goats goats
        , FullPadding.fromNoPadding <|
            Neat.div
                [ class "row-button"
                ]
                [ Neat.lift button
                    [ class "button"
                    , Mixin.fromAttribute <| Attributes.type_ "button"
                    , Mixin.fromAttribute <| Events.onClick RegisterAnotherGoat
                    ]
                    [ Neat.text "Register another goat"
                    ]
                ]
        ]



-- Form


registerForm_view : Bool -> Model -> View FullPadding Msg
registerForm_view verbose { registerForm } =
    let
        hasError : Goat.Error -> Bool
        hasError err =
            case Decoder.run Goat.decoder registerForm of
                Ok _ ->
                    False

                Err errs ->
                    List.member err errs
    in
    Goat.registerForm
        "goat-registerForm"
        verbose
        [ Neat.row
            [ FullPadding.fromNoPadding <| Goat.label "Name"
            , Neat.setMixin Mixin.expanded <|
                MiddlePadding.toFullPadding <|
                    Goat.control
                        [ MiddlePadding.fromNoPadding <|
                            Goat.fieldRequired
                                (hasError Goat.NameRequired)
                                [ "What's your name?"
                                ]
                        , MiddlePadding.fromNoPadding <|
                            Neat.setMixin Mixin.fullWidth <|
                                Input.view
                                    { placeholder = "Sakura-chan"
                                    , onChange = ChangeName
                                    }
                                    registerForm.name
                        , Goat.inputErrorField
                            Goat.Name.errorField
                            Goat.Name.decoder
                            registerForm.name
                        ]
            ]
        , Neat.row
            [ FullPadding.fromNoPadding <| Goat.label "Age"
            , Neat.setMixin Mixin.expanded <|
                MiddlePadding.toFullPadding <|
                    Goat.control
                        [ MiddlePadding.fromNoPadding <|
                            Goat.fieldRequired
                                (hasError Goat.AgeRequired)
                                [ "How old are you? [years old]"
                                ]
                        , MiddlePadding.fromNoPadding <|
                            Neat.setMixin Mixin.fullWidth <|
                                Input.view
                                    { placeholder = "2"
                                    , onChange = ChangeAge
                                    }
                                    registerForm.age
                        , Goat.inputErrorField
                            Goat.Age.errorField
                            Goat.Age.decoder
                            registerForm.age
                        ]
            ]
        , Neat.row
            [ FullPadding.fromNoPadding <| Goat.label "Horns"
            , Neat.setMixin Mixin.expanded <|
                MiddlePadding.toFullPadding <|
                    Goat.control
                        [ MiddlePadding.fromNoPadding <|
                            Goat.fieldRequired
                                (hasError Goat.HornsRequired)
                                [ "How many horns do you have?"
                                ]
                        , MiddlePadding.fromNoPadding <|
                            Neat.setMixin Mixin.fullWidth <|
                                Input.view
                                    { placeholder = "0"
                                    , onChange = ChangeHorns
                                    }
                                    registerForm.horns
                        , Goat.inputErrorField
                            Goat.Horns.errorField
                            Goat.Horns.decoder
                            registerForm.horns
                        ]
            ]
        , Neat.row
            [ FullPadding.fromNoPadding <| Goat.label "Means of contact"
            , Neat.setMixin Mixin.expanded <|
                MiddlePadding.toFullPadding <|
                    Goat.control
                        [ MiddlePadding.fromNoPadding <|
                            Goat.fieldRequired
                                (hasError Goat.ContactTypeRequired)
                                [ "How to contact you?"
                                ]
                        , MiddlePadding.fromNoPadding <|
                            Neat.setMixin Mixin.fullWidth <|
                                Select.view
                                    { options =
                                        ( Select.label "== Choose one ==", "" )
                                            :: List.map (\c -> ( ContactType.toLabel c, ContactType.toString c )) ContactType.enum
                                    , onChange = ChangeContactType
                                    }
                                    registerForm.contactType
                        , Goat.selectErrorField
                            ContactType.errorField
                            ContactType.decoder
                            registerForm.contactType
                        ]
            ]
        , Neat.setMixins
            [ class "toggle-field"
            , Mixin.boolAttribute "aria-hidden" <|
                Select.decodeField ContactType.decoder registerForm.contactType
                    /= Ok (Just ContactType.UseEmail)
            ]
            <| Neat.row
            [ FullPadding.fromNoPadding <| Goat.label "Email"
            , Neat.setMixin Mixin.expanded <|
                MiddlePadding.toFullPadding <|
                    Goat.control
                        [ MiddlePadding.fromNoPadding <|
                            Goat.fieldRequired
                                (hasError Goat.EmailRequired)
                                [ "Email address to contact you?"
                                ]
                        , MiddlePadding.fromNoPadding <|
                            Neat.setMixin Mixin.fullWidth <|
                                Input.view
                                    { placeholder = "you-goat-mail@example.com"
                                    , onChange = ChangeEmail
                                    }
                                    registerForm.email
                        , Goat.inputErrorField
                            Goat.Email.errorField
                            Goat.Email.decoder
                            registerForm.email
                        ]
            ]
        , Neat.setMixins
            [ class "toggle-field"
            , Mixin.boolAttribute "aria-hidden" <|
                Select.decodeField ContactType.decoder registerForm.contactType
                    /= Ok (Just ContactType.UsePhone)
            ]
            <| Neat.row
            [ FullPadding.fromNoPadding <| Goat.label "Phone number"
            , Neat.setMixin Mixin.expanded <|
                MiddlePadding.toFullPadding <|
                    Goat.control
                        [ MiddlePadding.fromNoPadding <|
                            Goat.fieldRequired
                                (hasError Goat.PhoneRequired)
                                [ "Phone number to contact you."
                                , "(Only Japanese-style mobile phone number)"
                                ]
                        , MiddlePadding.fromNoPadding <|
                            Neat.setMixin Mixin.fullWidth <|
                                Input.view
                                    { placeholder = "090-0000-0000"
                                    , onChange = ChangePhone
                                    }
                                    registerForm.phone
                        , Goat.inputErrorField
                            Goat.Phone.errorField
                            Goat.Phone.decoder
                            registerForm.phone
                        ]
            ]
        , Neat.row
            [ FullPadding.fromNoPadding <| Goat.label "Message"
            , Neat.setMixin Mixin.expanded <|
                MiddlePadding.toFullPadding <|
                    Goat.control
                        [ MiddlePadding.fromNoPadding <|
                            Goat.fieldOptional
                                [ "Any messages?"
                                ]
                        , MiddlePadding.fromNoPadding <|
                            Neat.setMixin Mixin.fullWidth <|
                                Input.view
                                    { placeholder = "Hi! I'm Sakura-chan."
                                    , onChange = ChangeMessage
                                    }
                                    registerForm.message
                        , Goat.inputErrorField
                            Goat.Message.errorField
                            Goat.Message.decoder
                            registerForm.message
                        ]
            ]
        , FullPadding.fromNoPadding <|
            Neat.div
                [ class "row-button"
                ]
                [ Neat.lift button
                    [ class "button"
                    , Mixin.fromAttribute <| Attributes.type_ "button"
                    , Mixin.fromAttribute <| Events.onClick SubmitRegister
                    ]
                    [ Neat.text "Register"
                    ]
                ]
        ]



-- Helper functions


{-| A specialized version of `class` for this module.
It handles generated class name by CSS modules.
-}
class : String -> Mixin msg
class =
    Css.classWithPrefix "app__"
