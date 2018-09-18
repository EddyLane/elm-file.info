port module Main exposing (..)

import Drag
import File.File as File
import File.List as FileList
import File.Upload as Upload
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)


---- PORTS ----


port browseClick : String -> Cmd msg


port uploadFile : String -> Cmd msg



---- MODEL ----


type alias Model =
    { requestId : Int
    , upload : Upload.State
    , readRequests : List File.FilePortRequest
    , responses : List File.FilePortResponse
    }


init : ( Model, Cmd Msg )
init =
    ( { upload = Upload.init
      , requestId = 1
      , readRequests = []
      , responses = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | OpenFileBrowser String
    | OnChangeFiles String (List Drag.File)
    | UploadFile File.FilePortRequest
    | OnFileRead (Result String File.FilePortResponse)
    | DragOver Drag.Event
    | DragLeave Drag.Event
    | Drop Drag.Event



-- | InsertResult JE.Value (Result Http.Error )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenFileBrowser inputID ->
            ( model, browseClick inputID )

        OnChangeFiles inputId files ->
            let
                readRequests =
                    File.requests (model.requestId + 1) inputId files
            in
            ( { model
                | readRequests = readRequests
                , requestId = model.requestId + List.length readRequests
              }
            , File.readCmds readRequests
            )

        UploadFile file ->
            ( model, Cmd.none )

        OnFileRead (Ok response) ->
            ( { model
                | responses = response :: model.responses
                , readRequests = File.removeRequest response model.readRequests
              }
            , Cmd.none
            )

        OnFileRead (Err err) ->
            ( model, Cmd.none )

        DragOver event ->
            ( { model | upload = Upload.dropActive True model.upload }
            , Cmd.none
            )

        DragLeave event ->
            ( { model | upload = Upload.dropActive False model.upload }
            , Cmd.none
            )

        Drop event ->
            let
                e =
                    Debug.log "e" event

                files_ =
                    List.map (\f -> Debug.log "file" f.data) (Debug.log "files" event.dataTransfer.files)

                files =
                    event.dataTransfer.files

                readRequests =
                    File.requests (model.requestId + 1) (Upload.getInputId uploadConfig) files
            in
            ( { model
                | readRequests = readRequests
                , requestId = model.requestId + List.length readRequests
                , upload = Upload.dropActive False model.upload
              }
            , File.readCmds readRequests
            )

        NoOp ->
            ( model, Cmd.none )


type alias SignedUpload =
    { signedUrl : String
    , reference : String
    }



-- uploadFiles : List Drag.Files


getSignedUploadUrl : Task e String
getSignedUploadUrl =
    Task.succeed "1234-5678-9101-1213"



-- uploadFile : String -> File.FilePortRequest -> Task e String
-- uploadFile file =
--     getSignedUploadUrl
--         |> Task.andThen
--             (\signedUploadUrl ->
--                 Task.succeed "test"
--             )
-- File.encoder file
-- |> always Cmd.none
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
    div [ style [ ( "width", "700px" ) ] ]
        [ Upload.view model.upload uploadConfig
        , FileList.view model.responses
        ]



---- SUBSCRIPTIONS ----


fileContentRead : List File.FilePortRequest -> Sub (Result String File.FilePortResponse)
fileContentRead requests =
    File.fileContentRead (Decode.decodeValue (File.filePortDecoder requests))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map OnFileRead (fileContentRead model.readRequests) ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
