module Calc exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev


type alias Knopf =
    { text : String
    , message : Message
    }


type alias Model =
    { anstehendeOperation : Float -> Float
    , aktuelleZahl : Float
    , stelle : Float
    }


type Message
    = ZifferGedrueck Float
    | OperatorGedrueck Operator


type Operator
    = Plus
    | Minus
    | Mal
    | Geteilt
    | Istgleich
    | Komma


main : Program Never Model Message
main =
    Html.beginnerProgram
        { model = Model identity 0 0
        , view = view
        , update = update
        }


update : Message -> Model -> Model
update msg model =
    case msg of
        ZifferGedrueck z ->
            if model.stelle == 0 then
                { model | aktuelleZahl = model.aktuelleZahl * 10 + z }
            else
                { model
                    | aktuelleZahl = model.aktuelleZahl + z * 10 ^ (-1 * model.stelle)
                    , stelle = model.stelle + 1
                }

        OperatorGedrueck op ->
            let
                ( neueZahl, neueOperation, neueStelle ) =
                    case op of
                        Istgleich ->
                            ( model.anstehendeOperation model.aktuelleZahl
                            , identity
                            , 0
                            )

                        Plus ->
                            ( 0
                            , \operand2 -> model.anstehendeOperation model.aktuelleZahl + operand2
                            , 0
                            )

                        Minus ->
                            ( 0
                            , \operand2 -> model.anstehendeOperation model.aktuelleZahl - operand2
                            , 0
                            )

                        Mal ->
                            ( 0
                            , \operand2 -> model.anstehendeOperation model.aktuelleZahl * operand2
                            , 0
                            )

                        Geteilt ->
                            ( 0
                            , \operand2 -> model.anstehendeOperation model.aktuelleZahl / operand2
                            , 0
                            )

                        Komma ->
                            ( model.aktuelleZahl
                            , model.anstehendeOperation
                            , if model.stelle == 0 then
                                1
                              else
                                model.stelle
                            )
            in
                { model
                    | aktuelleZahl = neueZahl
                    , anstehendeOperation = neueOperation
                    , stelle = neueStelle
                }


view : Model -> Html Message
view model =
    let
        zif x =
            Knopf (toString x) (ZifferGedrueck x)

        op s o =
            Knopf s (OperatorGedrueck o)

        opPl =
            op "+" Plus

        opMi =
            op "-" Minus

        opMa =
            op "*" Mal

        opDu =
            op "/" Geteilt

        opGl =
            op "=" Istgleich

        opKo =
            op "," Komma
    in
        Html.div [ Attr.style [ ( "width", "120px" ) ] ]
            [ display model
            , tastatur
                [ [ zif 7, zif 8, zif 9, opMa ]
                , [ zif 4, zif 5, zif 6, opDu ]
                , [ zif 1, zif 2, zif 3, opPl ]
                , [ zif 0, opKo, opGl, opMi ]
                ]
            ]


display : Model -> Html msg
display model =
    Html.h1
        [ Attr.style
            [ ( "background", "lightgray" )
            , ( "color", "black" )
            , ( "text-align", "right" )
            ]
        ]
        [ Html.text (toString model.aktuelleZahl) ]


tastatur : List (List Knopf) -> Html Message
tastatur knopfReihen =
    Html.div
        []
        (List.map knopfReihe knopfReihen)


knopfReihe : List Knopf -> Html Message
knopfReihe texte =
    Html.div
        []
        (List.map knopf texte)


knopf : Knopf -> Html Message
knopf knopf =
    Html.button
        [ Attr.style
            [ ( "width", "30px" )
            , ( "height", "30px" )
            ]
        , Ev.onClick knopf.message
        ]
        [ Html.text knopf.text ]
