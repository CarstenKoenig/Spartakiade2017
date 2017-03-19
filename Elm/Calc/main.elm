module Calc exposing (..)

import Html
import Html.Attributes as Attr
import Html.Events as Ev


-- Hallo


type alias Knopf =
    { text : String
    , message : Message
    }


type alias Model =
    { aktuelleZahl : Int
    , anstehendeOperation : Int -> Int
    }


type Message
    = ZifferGedrueckt Int
    | OperatorGedrueckt Operator


type Operator
    = Plus
    | Minus
    | Geteilt
    | Mal
    | IstGleich
    | Komma


main : Program Never Model Message
main =
    Html.beginnerProgram
        { model = Model 0 identity
        , view = view
        , update = update
        }


update : Message -> Model -> Model
update msg model =
    case msg of
        ZifferGedrueckt z ->
            { model
                | aktuelleZahl =
                    model.aktuelleZahl * 10 + z
            }

        OperatorGedrueckt IstGleich ->
            { model
                | aktuelleZahl =
                    model.anstehendeOperation
                        model.aktuelleZahl
                , anstehendeOperation =
                    identity
            }

        OperatorGedrueckt Plus ->
            { model
                | aktuelleZahl = 0
                , anstehendeOperation =
                    \x ->
                        model.anstehendeOperation model.aktuelleZahl
                            + x
            }

        _ ->
            model


view : Model -> Html.Html Message
view model =
    let
        zif z =
            Knopf (toString z) (ZifferGedrueckt z)

        op t o =
            Knopf t (OperatorGedrueckt o)
    in
        Html.div []
            [ display model.aktuelleZahl
            , tastatur
                [ [ zif 7, zif 8, zif 9, op "*" Mal ]
                , [ zif 4, zif 5, zif 6, op "/" Geteilt ]
                , [ zif 1, zif 2, zif 3, op "+" Plus ]
                , [ zif 0, op "," Komma, op "=" IstGleich, op "-" Minus ]
                ]
            ]


display : Int -> Html.Html Message
display zahl =
    Html.h1
        [ Attr.style
            [ ( "background", "lightgray" )
            , ( "color", "black" )
            , ( "text-align", "right" )
            , ( "width", "120px" )
            ]
        ]
        [ Html.text (toString zahl) ]


tastatur : List (List Knopf) -> Html.Html Message
tastatur knopfReihen =
    Html.div []
        (List.map knopfReihe knopfReihen)


knopfReihe : List Knopf -> Html.Html Message
knopfReihe texte =
    Html.div []
        (List.map (\text -> knopf text) texte)


knopf : Knopf -> Html.Html Message
knopf knopf =
    Html.button
        [ Attr.style
            [ ( "width", "30px" )
            , ( "height", "30px" )
            ]
        , Ev.onClick knopf.message
        ]
        [ Html.text knopf.text ]
