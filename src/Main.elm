module Main exposing (..)

import Drag
import File.File as File
import File.List as FileList
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
    | GotSignedUrl (Result Http.Error SignedUrl)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenFileBrowser inputID ->
            ( model, Upload.browseClick inputID )

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
            , Task.attempt GotSignedUrl getSignedUrl
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

        Drop { dataTransfer } ->
            let
                readRequests =
                    dataTransfer
                        |> .files
                        |> File.requests (model.requestId + 1) (Upload.getInputId uploadConfig)
            in
            ( { model
                | readRequests = readRequests
                , requestId = model.requestId + List.length readRequests
                , upload = Upload.dropActive False model.upload
              }
            , File.readCmds readRequests
            )

        GotSignedUrl (Ok { signedUrl }) ->
            ( model, Cmd.none )

        GotSignedUrl (Err e) ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


type alias SignedUrl =
    { signedUrl : String
    , reference : String
    }


getSignedUrl : Task Http.Error SignedUrl
getSignedUrl =
    Http.get signedUrlProviderUrl
        (Pipeline.decode SignedUrl
            |> Pipeline.required "signedUrl" Decode.string
            |> Pipeline.required "reference" Decode.string
        )
        |> Http.toTask



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
    div [ style [ ( "width", "700px" ), ( "border", "1px solid #000" ) ] ]
        [ Upload.view model.upload uploadConfig
        , hr [] []
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
