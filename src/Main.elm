module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

--import Page.Demo as ComplexDemo

import Browser
import Html exposing (Html, text)
import Http
import Page.DemoBasic as Demo
import Task



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
    case model of
        Loaded subModel ->
            { title = "Elm File demo #1"
            , body = [ Html.map DemoMsg (Demo.view subModel) ]
            }

        Loading ->
            { title = "Elm File demo #1 (loading)"
            , body = [ text "Loading..." ]
            }

        Errored ->
            { title = "Elm File demo #1 (error)"
            , body = [ text "Something went wrong" ]
            }



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
