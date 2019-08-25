module MenuItem exposing
    ( MenuItem(..)
    , enum
    , toClassname
    , toLabel
    )


type MenuItem
    = Home
    | Explore
    | Notifications
    | Messages
    | Bookmarks
    | Lists
    | More


enum : List MenuItem
enum =
    [ Home
    , Explore
    , Notifications
    , Messages
    , Bookmarks
    , Lists
    , More
    ]


toClassname : MenuItem -> String
toClassname menu =
    "menuItem-"
        ++ (case menu of
                Home ->
                    "home"

                Explore ->
                    "explore"

                Notifications ->
                    "notifications"

                Messages ->
                    "messages"

                Bookmarks ->
                    "bookmarks"

                Lists ->
                    "lists"

                More ->
                    "more"
           )


toLabel : MenuItem -> String
toLabel menu =
    case menu of
        Home ->
            "Home"

        Explore ->
            "Explore"

        Notifications ->
            "Notifications"

        Messages ->
            "Messages"

        Bookmarks ->
            "Bookmarks"

        Lists ->
            "Lists"

        More ->
            "More"
