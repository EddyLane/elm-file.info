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
    { requestId : Int
    , upload : Upload.State
    , reading : List File.FileReadPortRequest
    , signing : List File.FileReadPortResponse
    , uploading : List File.FileSigned
    }


init : ( Model, Cmd Msg )
init =
    ( { upload = Upload.init
      , requestId = 1
      , reading = []
      , signing = []
      , uploading = []
      }
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
            ( { model | upload = Upload.dropActive True model.upload }
            , Cmd.none
            )

        DragLeave event ->
            ( { model | upload = Upload.dropActive False model.upload }
            , Cmd.none
            )

        OpenFileBrowser inputID ->
            ( model, Upload.browseClick inputID )

        OnChangeFiles inputId files ->
            let
                reading =
                    File.requests (model.requestId + 1) inputId files
            in
            ( { model
                | reading = reading
                , requestId = model.requestId + List.length reading
              }
            , File.readCmds reading
            )

        Drop { dataTransfer } ->
            let
                reading =
                    dataTransfer
                        |> .files
                        |> File.requests (model.requestId + 1) (Upload.getInputId uploadConfig)
            in
            ( { model
                | reading = reading
                , requestId = model.requestId + List.length reading
                , upload = Upload.dropActive False model.upload
              }
            , File.readCmds reading
            )

        OnFileRead (Ok response) ->
            ( { model
                | signing = response :: model.signing
                , reading = File.removeReadRequest response model.reading
              }
            , Task.attempt (GotSignedUrl response) getSignedUrl
            )

        OnFileRead (Err err) ->
            ( model, Cmd.none )

        GotSignedUrl response (Ok signedUrl) ->
            let
                signed =
                    File.signed response signedUrl
            in
            ( { model
                | signing = File.removeSigningRequest signed model.signing
                , uploading = signed :: model.uploading
              }
            , File.uploadCmds [ signed ]
            )

        GotSignedUrl _ (Err e) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
        [ Upload.view model.upload uploadConfig
        , hr [] []
        , FileList.view
            { reading = model.reading
            , signing = model.signing
            , uploading = model.uploading
            }
        ]



---- SUBSCRIPTIONS ----


fileContentRead : List File.FileReadPortRequest -> Sub (Result String File.FileReadPortResponse)
fileContentRead requests =
    File.fileContentRead (Decode.decodeValue (File.filePortDecoder requests))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map OnFileRead (fileContentRead model.reading) ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
