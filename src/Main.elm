module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import FeatherIcons
import Html
import Http
import Page.DemoBasic as Demo
import Task
import View.Helpers as View



---- MODEL ----


type Model
    = Loaded Demo.Model
    | Loading
    | Errored


init : ( Model, Cmd Msg )
init =
    ( Loading
    , Task.attempt DemoLoaded Demo.init
    )


type Msg
    = DemoLoaded (Result Http.Error Demo.Model)
    | DemoMsg Demo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( DemoLoaded (Ok subModel), _ ) ->
            ( Loaded subModel
            , Cmd.none
            )

        ( DemoLoaded (Err _), _ ) ->
            ( Errored
            , Cmd.none
            )

        ( DemoMsg subMsg, Loaded subModel ) ->
            Demo.update subMsg subModel
                |> Tuple.mapFirst Loaded
                |> Tuple.mapSecond (Cmd.map DemoMsg)

        _ ->
            ( model
            , Cmd.none
            )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = title model
    , body =
        [ layout
            [ width fill
            , height fill
            , Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Roboto"
                    , name = "Roboto"
                    }
                , Font.sansSerif
                ]
            ]
            (column
                [ width fill
                , height fill
                ]
                [ row
                    [ Background.color (rgba 0 0 0 1)
                    , Font.color (rgba 1 1 1 1)
                    , height (px 56)
                    , width fill
                    , alignTop
                    ]
                    [ text "This is the header" ]
                , el
                    [ width fill
                    , height fill
                    ]
                    (body model)
                ]
            )
        ]
    }


title : Model -> String
title model =
    case model of
        Loaded _ ->
            "Elm File demo"

        Loading ->
            "Elm File demo (loading)"

        Errored ->
            "Elm File demo (error)"


body : Model -> Element Msg
body model =
    case model of
        Loaded subModel ->
            Demo.view subModel
                |> map DemoMsg

        Loading ->
            View.icon FeatherIcons.loader

        Errored ->
            View.icon FeatherIcons.alertTriangle



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loaded subModel ->
            Demo.subscriptions subModel
                |> Sub.map DemoMsg

        Loading ->
            Sub.none

        Errored ->
            Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = always init
        , update = update
        , subscriptions = subscriptions
        }
