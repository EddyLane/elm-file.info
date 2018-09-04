module Main exposing (..)

import Drag
import File.File as File
import File.Upload as Upload
import Html exposing (..)


---- MODEL ----


type alias Model =
    { upload : Upload.State }


init : ( Model, Cmd Msg )
init =
    ( { upload = Upload.init }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | OpenFileBrowser
    | OnChangeFiles (List Drag.File)
    | UploadFile File.FileData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateUpload =
            Upload.update model
    in
    case msg of
        OpenFileBrowser ->
            updateUpload Upload.openFileBrowser

        OnChangeFiles files ->
            updateUpload (Upload.fileChanges files)

        UploadFile file ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Upload.config NoOp
        |> Upload.maximumFileSize 500
        |> Upload.browseFiles OpenFileBrowser
        |> Upload.backendUrl "http://localhost:4000"
        |> Upload.uploadFile UploadFile
        |> Upload.view model.upload



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
