port module Main exposing (..)

import Drag
import File.File as File
import File.Upload as Upload
import Html exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)


---- PORTS ----


port browseClick : () -> Cmd msg


port uploadFile : String -> Cmd msg



---- MODEL ----


type alias Model =
    { requestId : Int
    , upload : Upload.State
    , requests : List File.FilePortRequest
    , responses : List File.FilePortResponse
    }


init : ( Model, Cmd Msg )
init =
    ( { upload = Upload.init
      , requestId = 1
      , requests = []
      , responses = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | OpenFileBrowser
    | OnChangeFiles (List Drag.File)
    | UploadFile File.FilePortRequest
    | OnFileRead (Result String File.FilePortResponse)



-- | InsertResult JE.Value (Result Http.Error )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenFileBrowser ->
            ( model, browseClick () )

        OnChangeFiles files ->
            let
                requests =
                    filesToPortRequests model.requestId files
            in
            ( { model
                | requests = requests
                , requestId = model.requestId + List.length requests
              }
            , requests
                |> List.map (File.encoder >> Encode.encode 0 >> File.readFileContent)
                |> Cmd.batch
            )

        UploadFile file ->
            ( model, Cmd.none )

        OnFileRead (Ok response) ->
            ( { model | responses = response :: model.responses }
            , Cmd.none
            )

        OnFileRead (Err err) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


type alias SignedUpload =
    { signedUrl : String
    , reference : String
    }


filesToPortRequests : Int -> List Drag.File -> List File.FilePortRequest
filesToPortRequests requestId files =
    File.requests (requestId + 1) files



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
        |> Upload.backendUrl "http://localhost:4000"
        |> Upload.maximumFileSize 500
        |> Upload.onChangeFiles OnChangeFiles
        |> Upload.browseFiles OpenFileBrowser
        |> Upload.uploadFile UploadFile


view : Model -> Html Msg
view model =
    Upload.view model.upload uploadConfig



---- SUBSCRIPTIONS ----


fileContentRead : Sub (Result String File.FilePortResponse)
fileContentRead =
    File.fileContentRead (Decode.decodeValue File.filePortDecoder)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map OnFileRead fileContentRead ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
