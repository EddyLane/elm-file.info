module Main exposing (..)

import Drag
import File.File as File
import File.List as FileList
import File.SignedUrl as SignedUrl exposing (SignedUrl)
import File.Upload as Upload
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Task exposing (Task)


signedUrlProviderUrl : String
signedUrlProviderUrl =
    "http://localhost:3003/signed-upload-url"



---- MODEL ----


type alias Model =
    Upload.State


init : ( Model, Cmd Msg )
init =
    ( Upload.init
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | OpenFileBrowser String
    | OnChangeFiles String (List Drag.File)
    | OnFileRead (Result String File.FileReadPortResponse)
    | DragOver Drag.Event
    | DragLeave Drag.Event
    | Drop Drag.Event
    | GotSignedUrl File.FileReadPortResponse (Result Http.Error SignedUrl)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragOver event ->
            ( Upload.dropActive True model
            , Cmd.none
            )

        DragLeave event ->
            ( Upload.dropActive False model
            , Cmd.none
            )

        OpenFileBrowser inputID ->
            ( model
            , Upload.browseClick inputID
            )

        OnChangeFiles _ files ->
            Upload.addFileReadRequests files model

        Drop { dataTransfer } ->
            Upload.addFileReadRequests dataTransfer.files model

        OnFileRead (Ok file) ->
            ( Upload.fileReadSuccess file model
            , Task.attempt (GotSignedUrl file) getSignedUrl
            )

        OnFileRead (Err err) ->
            ( model
            , Cmd.none
            )

        GotSignedUrl file (Ok signedUrl) ->
            Upload.getSignedS3UrlSuccess signedUrl file model

        GotSignedUrl _ (Err e) ->
            ( model
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


getSignedUrl : Task Http.Error SignedUrl
getSignedUrl =
    Http.get signedUrlProviderUrl SignedUrl.decoder
        |> Http.toTask



---- VIEW ----


uploadConfig : Upload.Config Msg
uploadConfig =
    Upload.config NoOp
        |> Upload.maximumFileSize 500
        |> Upload.onChangeFiles OnChangeFiles
        |> Upload.browseFiles OpenFileBrowser
        |> Upload.drag DragOver DragLeave Drop
        |> Upload.inputId "elm-file-example"


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "width", "700px" )
            , ( "border", "1px solid #000" )
            ]
        ]
        [ Upload.view model uploadConfig
        , hr [] []
        , FileList.view model
        ]



---- SUBSCRIPTIONS ----


fileContentRead : List File.FileReadPortRequest -> Sub (Result String File.FileReadPortResponse)
fileContentRead requests =
    File.fileContentRead (Decode.decodeValue (File.filePortDecoder requests))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map OnFileRead (fileContentRead <| Upload.getReading model) ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
