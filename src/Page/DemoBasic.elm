port module Page.DemoBasic exposing (Model, Msg, init, subscriptions, update, view)

import Date exposing (Date)
import Date.Extra
import Drag
import File.Data.UploadId as UploadId exposing (UploadId)
import File.DropZone as DropZone
import File.FileList as FileList
import File.Upload as Upload
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import List as FileList
import Ports.DropZone
import Ports.Upload
import Task exposing (Task)


attachmentCollectionUrl : String
attachmentCollectionUrl =
    "http://localhost:3003/attachments"


attachmentUrl : Attachment -> String
attachmentUrl { reference } =
    "http://localhost:3003/attachments/" ++ reference



---- MODEL ----


type alias Model =
    { dropZone : DropZone.State
    , upload : Upload.State
    , files : List Attachment
    , list : FileList.State ()
    }


init : Task Http.Error Model
init =
    Task.map initialModel loadAttachments


initialModel : List Attachment -> Model
initialModel files =
    { dropZone = DropZone.init
    , upload = Upload.init
    , files = files
    , list = FileList.init listConfig
    }


uploadConfig : Upload.Config Msg
uploadConfig =
    Upload.config NoOp
        |> Upload.configSetStateMsg SetUploadState
        |> Upload.configMaximumFileSize 2
        |> Upload.configUploadedMsg Uploaded
        |> Upload.configBase64EncodedMsg EncodeFile


dropZoneConfig : DropZone.Config Msg
dropZoneConfig =
    DropZone.config NoOp
        |> DropZone.configSetState SetDropZoneState
        |> DropZone.configUploadFiles UploadFiles
        |> DropZone.configBrowseFiles OpenFileBrowser
        |> DropZone.configAttrs dropZoneAttrs
        |> DropZone.configContents dropZoneContents


listConfig : FileList.Config () Attachment Msg
listConfig =
    FileList.config NoOp
        |> FileList.configListStateMsg SetListState
        |> FileList.configIdFn .reference
        |> FileList.configNameFn .fileName
        |> FileList.configContentTypeFn .contentType
        |> FileList.configThumbnailSrcFn (.reference >> (++) "http://localhost:3003/attachments/")


loadAttachments : Task Http.Error (List Attachment)
loadAttachments =
    Http.get attachmentCollectionUrl (Decode.list attachmentDecoder)
        |> Http.toTask


type alias Attachment =
    { reference : String
    , contentType : String
    , fileName : String
    }


attachmentDecoder : Decode.Decoder Attachment
attachmentDecoder =
    Pipeline.decode Attachment
        |> Pipeline.required "reference" Decode.string
        |> Pipeline.required "mimetype" Decode.string
        |> Pipeline.required "fileName" Decode.string


fileEncoder : Drag.File -> Encode.Value
fileEncoder file =
    Encode.object
        [ ( "contentType", Encode.string file.typeMIME )
        , ( "fileName", Encode.string file.name )
        ]



---- UPDATE ----


type Msg
    = NoOp
    | SetDropZoneState DropZone.State
    | SetListState (FileList.State ())
    | SetUploadState Upload.State
    | OpenFileBrowser String
    | UploadFiles (List Drag.File)
    | EncodeFile (Result UploadId ( UploadId, Upload.UploadingFile ))
    | Uploaded (Result UploadId ( UploadId, Encode.Value ))
    | CancelUpload UploadId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUploadState upload ->
            ( { model | upload = upload }
            , Cmd.none
            )

        SetListState list ->
            ( { model | list = list }
            , Cmd.none
            )

        SetDropZoneState dropZone ->
            ( { model | dropZone = dropZone }
            , Cmd.none
            )

        OpenFileBrowser inputID ->
            ( model
            , Ports.DropZone.openFileBrowser inputID
            )

        UploadFiles files ->
            let
                ( upload, base64Cmd ) =
                    Upload.encode uploadSubs uploadConfig files model.upload
            in
            ( { model | upload = upload }
            , base64Cmd
            )

        EncodeFile (Ok ( id, file )) ->
            let
                updatedUploadState =
                    Upload.update id file model.upload

                uploadUrl =
                    Encode.string attachmentCollectionUrl

                additionalData =
                    Encode.string (Upload.fileFilename file)
            in
            ( { model | upload = updatedUploadState }
            , Upload.upload uploadSubs uploadUrl additionalData id updatedUploadState
            )

        EncodeFile (Err uploadId) ->
            ( { model | upload = Upload.failure uploadId model.upload }
            , Cmd.none
            )

        CancelUpload file ->
            model.upload
                |> Upload.cancel uploadSubs file
                |> Tuple.mapFirst (\upload -> { model | upload = upload })

        Uploaded (Ok ( uploadId, encodedAttachment )) ->
            let
                files =
                    encodedAttachment
                        |> Decode.decodeValue attachmentDecoder
                        |> Result.map (\file -> file :: model.files)
                        |> Result.withDefault model.files
            in
            ( { model
                | upload = Upload.success uploadId model.upload
                , files = files
              }
            , Cmd.none
            )

        Uploaded (Err uploadId) ->
            ( { model | upload = Upload.failure uploadId model.upload }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { upload, files, list, dropZone } =
    div
        [ class "container my-4" ]
        [ div [ class "row" ]
            [ div [ class "col card" ]
                [ DropZone.view dropZone dropZoneConfig
                , FileList.view list upload files listConfig
                ]
            ]
        ]


dropZoneContents : DropZone.State -> Msg -> List (Html Msg)
dropZoneContents _ openFileBrowser =
    [ h2 [] [ text "Files" ]
    , span []
        [ i [ class "fas fa-upload" ] []
        , text "Drop your files here or "
        , a [ onClick openFileBrowser ] [ text "browse for a file" ]
        , text " to upload."
        ]
    ]


dropZoneAttrs : DropZone.State -> List (Attribute Msg)
dropZoneAttrs dropzoneState =
    [ style
        [ ( "width", "100%" )
        , ( "height", "150px" )
        , ( "border-bottom"
          , if DropZone.isActive dropzoneState then
                "2px dashed #ddd"
            else
                "2px dashed transparent"
          )
        , ( "background"
          , if DropZone.isActive dropzoneState then
                "#dff0d8"
            else
                "#f7f7f7"
          )
        ]
    ]



---- SUBSCRIPTIONS ----


uploadSubs : Upload.SubsConfig Msg
uploadSubs =
    { uploadProgress = Ports.Upload.uploadProgress
    , uploadCancelled = Ports.Upload.uploadCancelled
    , readFileContent = Ports.Upload.readFileContent
    , fileContentReadFailed = Ports.Upload.fileContentReadFailed
    , fileContentRead = Ports.Upload.fileContentRead
    , uploadPort = Ports.Upload.uploadPort
    , uploadFailed = Ports.Upload.uploadFailed
    , uploaded = Ports.Upload.uploaded
    }


subscriptions : Model -> Sub Msg
subscriptions { upload } =
    Upload.subscriptions
        { state = upload
        , config = uploadConfig
        , subscriptions = uploadSubs
        }
