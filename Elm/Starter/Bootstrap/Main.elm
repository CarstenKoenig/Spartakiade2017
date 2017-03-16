module Main exposing (main)

import Platform.Sub as Subs
import Html exposing (Html, div, text, button)
import Html.Attributes as Attr
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN


type alias Model =
    ()


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( ()
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            () ! [ Cmd.none ]


view : Model -> Html Msg
view () =
    Grid.container []
        [ CDN.stylesheet
          -- creates an inline style node with the Bootstrap CSS
        , div [ Attr.class "jumbotron" ] [ Html.h1 [] [ text "Hallo Elm " ] ]
        , Grid.row []
            [ Grid.col []
                [ text "Some content for my view here..." ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Subs.none


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }
