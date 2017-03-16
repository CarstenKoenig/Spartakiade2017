module Main exposing (main)

import Platform.Sub as Subs
import Html as Html exposing (Html, div, text, button)
import Html.Attributes as Attr
import Html.Events as Ev
import Http exposing (Error)
import Time
import Time.ZonedDateTime as ZTime exposing (ZonedDateTime)
import Time.TimeZones exposing (europe_berlin)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Badge as Badge
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Form.Input as Input
import Bootstrap.Table as Table
import Kommunikation as Komm exposing (SpielRunde, Spieler, Versuch, EigenerVersuch)


type alias Model =
    { runde : Runde
    , flags : Flags
    , spieler : Maybe Spieler
    , punkte : List Versuch
    , loginForm : LoginForm
    , zeit : Time.Time
    }


type alias LoginForm =
    { nameInput : String
    }


type alias LaufendeRunde =
    { vorgabe : SpielRunde
    , formelInput : String
    , endetUm : ZonedDateTime
    , letzterVersuch : VersuchsAusgang
    }


type VersuchsAusgang
    = Keiner
    | FormelFehler String
    | ErfolgreicherVersuch EigenerVersuch


type Runde
    = Wartet ZonedDateTime
    | Läuft LaufendeRunde


type alias Flags =
    { websocketUrl : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        runde =
            Wartet (ZTime.zonedDateTime (europe_berlin ()) ZTime.zero)

        loginForm =
            LoginForm ""
    in
        Model runde flags Nothing [] loginForm 0
            ! [ Komm.getSpielZustand HttpError UpdateZustand
              , Komm.getLogin (always Logout) SetSpieler
              ]


type Msg
    = HttpError Error
    | UpdateZustand Komm.SpielZustand
    | Login String
    | SetSpieler Spieler
    | Logout
    | SendeVersuch String
    | VersuchResult (Result String EigenerVersuch)
    | InputName String
    | InputFormel String
    | Tick Time.Time
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! [ Cmd.none ]

        Tick zeit ->
            { model | zeit = zeit } ! [ Komm.getSpielZustand HttpError UpdateZustand ]

        HttpError fehler ->
            model
                ! [ Cmd.none ]
                |> Debug.log (toString fehler)

        UpdateZustand zustand ->
            let
                ( formel, ltVersuch ) =
                    case model.runde of
                        Läuft l ->
                            ( l.formelInput, l.letzterVersuch )

                        _ ->
                            ( "", Keiner )

                ( runde, punkte ) =
                    case zustand.runde of
                        Just r ->
                            let
                                laufende =
                                    LaufendeRunde r formel zustand.endetUm ltVersuch
                            in
                                ( Läuft laufende, r.versuche )

                        Nothing ->
                            ( Wartet zustand.endetUm, model.punkte )
            in
                { model | runde = runde, punkte = punkte } ! [ Cmd.none ]

        SetSpieler spieler ->
            { model | spieler = Just spieler } ! [ Cmd.none ]

        Logout ->
            { model | spieler = Nothing } ! [ Cmd.none ]

        Login login ->
            { model | spieler = Nothing }
                ! [ Komm.postLogin login (always Logout) SetSpieler ]

        SendeVersuch formel ->
            model
                ! [ Komm.postVersuch formel HttpError VersuchResult ]

        VersuchResult (Result.Err fehler) ->
            let
                updated =
                    case model.runde of
                        Wartet _ ->
                            model

                        Läuft l ->
                            let
                                upd =
                                    { l | letzterVersuch = FormelFehler fehler }
                            in
                                { model | runde = Läuft upd }
            in
                updated ! [ Komm.getSpielZustand HttpError UpdateZustand ]

        VersuchResult (Result.Ok versuch) ->
            let
                updated =
                    case model.runde of
                        Wartet _ ->
                            model

                        Läuft l ->
                            let
                                upd =
                                    { l | letzterVersuch = ErfolgreicherVersuch versuch }
                            in
                                { model | runde = Läuft upd }
            in
                updated ! [ Komm.getSpielZustand HttpError UpdateZustand ]

        InputName name ->
            let
                updated =
                    LoginForm name
            in
                { model | loginForm = updated } ! [ Cmd.none ]

        InputFormel formel ->
            let
                updated =
                    case model.runde of
                        Wartet _ ->
                            model

                        Läuft l ->
                            let
                                upd =
                                    { l | formelInput = formel }
                            in
                                { model | runde = Läuft upd }
            in
                updated ! [ Cmd.none ]


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
          -- creates an inline style node with the Bootstrap CSS
        , div [ Attr.class "jumbotron" ] [ Html.h1 [] [ text "Let's play Countdown" ] ]
        , Grid.row []
            [ Grid.col [] [ viewMain model ]
            , Grid.col [ Col.xs, Col.lg4 ] [ viewScore model ]
            ]
        ]


viewScore : Model -> Html Msg
viewScore model =
    let
        versuchTr versuch =
            Table.tr []
                [ Table.td [] [ text versuch.spieler.nick ]
                , Table.td [] [ text (Maybe.map toString versuch.wert |> Maybe.withDefault "---") ]
                , Table.td [] [ text (toString versuch.punkte) ]
                ]

        punkteTr =
            List.map versuchTr model.punkte
    in
        Table.table
            { options = [ Table.striped, Table.hover ]
            , thead =
                Table.simpleThead
                    [ Table.th [] [ text "Spieler" ]
                    , Table.th [] [ text "Wert" ]
                    , Table.th [] [ text "Punkte" ]
                    ]
            , tbody =
                Table.tbody [] punkteTr
            }


viewMain : Model -> Html Msg
viewMain model =
    case model.spieler of
        Nothing ->
            viewLogin model

        Just spieler ->
            viewSpiel spieler model


viewLogin : Model -> Html Msg
viewLogin model =
    Form.formInline [ Ev.onSubmit (Login model.loginForm.nameInput) ]
        [ Input.text
            [ Input.attrs
                [ Attr.placeholder "Name..."
                , Ev.onInput InputName
                ]
            ]
        , Button.button
            [ Button.primary
            , Button.attrs
                [ Attr.class "ml-sm-2 my-2"
                  -- , Ev.onClick (Login model.loginForm.nameInput)
                ]
            ]
            [ text "Login" ]
        ]


viewSpiel : Spieler -> Model -> Html Msg
viewSpiel spieler model =
    case model.runde of
        Läuft runde ->
            viewRunde (verbleibendeSekunden model) runde

        Wartet _ ->
            viewWartet (verbleibendeSekunden model)


viewWartet : String -> Html Msg
viewWartet sekunden =
    Card.config []
        |> Card.block []
            [ Card.text []
                [ text "Nächste Runde in: "
                , Html.strong [] [ text sekunden ]
                ]
            ]
        |> Card.view


viewRunde : String -> LaufendeRunde -> Html Msg
viewRunde sekunden runde =
    Card.config []
        |> Card.header []
            [ text "Herausforderung: "
            , Html.strong [] [ text (toString runde.vorgabe.herausforderung.zielZahl) ]
            , text " - endet in: "
            , Html.strong [] [ text sekunden ]
            ]
        |> Card.block []
            [ Card.titleH5 [] [ text "Zahlenvorat" ]
            , Card.text []
                [ Html.p []
                    (runde.vorgabe.herausforderung.zahlenvorat
                        |> List.map (toString >> text >> List.singleton >> Badge.pillInfo [ Attr.class "ml-1" ])
                    )
                , viewFormelEingabe runde
                , viewVersuchsAusgang runde.letzterVersuch
                ]
            ]
        |> Card.view


viewFormelEingabe : LaufendeRunde -> Html Msg
viewFormelEingabe runde =
    Form.formInline [ Ev.onSubmit (SendeVersuch runde.formelInput) ]
        [ Input.text
            [ Input.attrs
                [ Attr.placeholder "Formel..."
                , Ev.onInput InputFormel
                ]
            ]
        , Button.button
            [ Button.primary
            , Button.attrs
                [ Attr.class "ml-sm-2 my-2"
                ]
            ]
            [ text "OK" ]
        ]


viewVersuchsAusgang : VersuchsAusgang -> Html Msg
viewVersuchsAusgang ausgang =
    case ausgang of
        Keiner ->
            div [] []

        ErfolgreicherVersuch versuch ->
            viewVersuch versuch

        FormelFehler fehler ->
            div []
                [ Alert.danger [ text fehler ]
                ]


viewVersuch : EigenerVersuch -> Html Msg
viewVersuch versuch =
    let
        wert =
            case versuch.wert of
                Just w ->
                    toString w

                Nothing ->
                    "---"
    in
        div []
            [ Alert.success
                [ text ("ergibt " ++ wert)
                , Badge.badge
                    [ Attr.class "ml-1" ]
                    [ text (toString versuch.punkte) ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Komm.websocketSub model.flags.websocketUrl (\_ -> NoOp) (UpdateZustand)
        , Time.every Time.second Tick
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }



-------------------------------
-- Hilfsfunktionen


verbleibendeSekunden : Model -> String
verbleibendeSekunden model =
    let
        sekDiff z =
            (ZTime.toTimestamp z - model.zeit)
                |> Time.inSeconds
                |> ceiling
                |> (\z -> toString z ++ "s")
    in
        if model.zeit == 0 then
            "---"
        else
            case model.runde of
                Wartet z ->
                    sekDiff z

                Läuft l ->
                    sekDiff l.endetUm
