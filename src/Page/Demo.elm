module Page.Demo exposing (Model, Msg, init, subscriptions, update, view)

import Date exposing (Date)
import Date.Extra
import Drag
import File.List as FileList
import File.SignedUrl as SignedUrl exposing (SignedUrl)
import File.Upload as Upload
import File.UploadId as UploadId exposing (UploadId)
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


getAttachmentsUrl : String
getAttachmentsUrl =
    "http://localhost:3003/get-attachments"



---- MODEL ----


type alias Model =
    { upload : Upload.State Attachment
    , list : FileList.State ColumnId
    , files : List Attachment
    }


init : Task Http.Error Model
init =
    Task.map (Model Upload.init (FileList.init fileListConfig)) loadAttachments


loadAttachments : Task Http.Error (List Attachment)
loadAttachments =
    Http.get getAttachmentsUrl (Decode.list attachmentDecoder)
        |> Http.toTask



{--

A representation of your business logic data type "File" as specified by what you receive from the backend
--}


type alias AttachmentResponse =
    { attachment : Attachment
    , signedUrl : SignedUrl
    }


type alias Attachment =
    { uploadedAt : Date
    , id : Int
    , reference : String
    , contentType : String
    , fileName : String
    , uploadedBy : String
    }


attachmentDecoder : Decode.Decoder Attachment
attachmentDecoder =
    Decode.field "date" Decode.string
        |> Decode.andThen
            (\uploadedAt ->
                case Date.fromString uploadedAt of
                    Ok date ->
                        Pipeline.decode (Attachment date)
                            |> Pipeline.required "id" Decode.int
                            |> Pipeline.required "reference" Decode.string
                            |> Pipeline.required "contentType" Decode.string
                            |> Pipeline.required "fileName" Decode.string
                            |> Pipeline.required "uploadedBy" Decode.string

                    Err _ ->
                        Decode.fail "Could not parse date"
            )



---- UPDATE ----


type Msg
    = NoOp
    | OpenFileBrowser String
    | InputFiles String (List Drag.File)
    | Base64EncodedFile (Result String ( UploadId, Upload.UploadingFile Attachment ))
    | DragFilesOver Drag.Event
    | DragFilesLeave Drag.Event
    | DropFiles Drag.Event
    | GotSignedS3Url UploadId (Result Http.Error AttachmentResponse)
    | UploadedFile (Result String UploadId)
    | UploadProgress UploadId Float
    | CancelUpload UploadId
    | SetListState (FileList.State ColumnId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragFilesOver _ ->
            ( { model | upload = Upload.dropActive True model.upload }
            , Cmd.none
            )

        DragFilesLeave _ ->
            ( { model | upload = Upload.dropActive False model.upload }
            , Cmd.none
            )

        OpenFileBrowser inputID ->
            ( model
            , Upload.browseClick inputID
            )

        DropFiles { dataTransfer } ->
            let
                ( upload, base64Cmd ) =
                    model.upload
                        |> Upload.dropActive False
                        |> Upload.base64EncodeFiles dataTransfer.files
            in
            ( { model | upload = upload }
            , base64Cmd
            )

        InputFiles _ files ->
            let
                ( upload, base64Cmd ) =
                    model.upload
                        |> Upload.dropActive False
                        |> Upload.base64EncodeFiles files
            in
            ( { model | upload = upload }
            , base64Cmd
            )

        Base64EncodedFile (Ok ( id, file )) ->
            ( { model | upload = Upload.fileReadSuccess id file model.upload }
            , Task.attempt (GotSignedS3Url id) (getSignedUrl file)
            )

        Base64EncodedFile (Err err) ->
            ( model
            , Cmd.none
            )

        GotSignedS3Url file (Ok { attachment, signedUrl }) ->
            let
                ( upload, uploadCmd ) =
                    Upload.uploadFileToSignedUrl signedUrl attachment file model.upload
            in
            ( { model | upload = upload }
            , uploadCmd
            )

        GotSignedS3Url _ (Err e) ->
            ( model
            , Cmd.none
            )

        UploadProgress requestId progress ->
            ( { model | upload = Upload.updateS3UploadProgress requestId progress model.upload }
            , Cmd.none
            )

        CancelUpload file ->
            let
                ( upload, cancelCmd ) =
                    Upload.cancelUpload file model.upload
            in
            ( { model | upload = upload }
            , cancelCmd
            )

        UploadedFile (Ok requestId) ->
            let
                ( upload, maybeAttachment ) =
                    Upload.fileUploadSuccess requestId model.upload

                files =
                    maybeAttachment
                        |> Maybe.map (\attachment -> attachment :: model.files)
                        |> Maybe.withDefault model.files
            in
            ( { model
                | upload = upload
                , files = files
              }
            , Cmd.none
            )

        UploadedFile (Err e) ->
            ( model
            , Cmd.none
            )

        SetListState list ->
            ( { model | list = list }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


getSignedUrl : Upload.UploadingFile file -> Task Http.Error AttachmentResponse
getSignedUrl file =
    Http.post signedUrlProviderUrl
        (Http.jsonBody <| Upload.signedUrlMetadataEncoder file)
        (Pipeline.decode AttachmentResponse
            |> Pipeline.required "attachment" attachmentDecoder
            |> Pipeline.required "signedUrl" SignedUrl.decoder
        )
        |> Http.toTask



---- VIEW ----


uploadConfig : Upload.Config Msg
uploadConfig =
    Upload.config NoOp
        |> Upload.maximumFileSize 500
        |> Upload.onChangeFiles InputFiles
        |> Upload.browseFiles OpenFileBrowser
        |> Upload.drag DragFilesOver DragFilesLeave DropFiles
        |> Upload.inputId "elm-file-example"


type ColumnId
    = UploadedOn
    | UploadedBy


fileListConfig : FileList.Config ColumnId Attachment Msg
fileListConfig =
    FileList.config NoOp
        |> FileList.idFn .reference
        |> FileList.nameFn .fileName
        |> FileList.contentTypeFn .contentType
        |> FileList.thumbnailSrcFn (.reference >> (++) "http://localhost:3003/download/")
        |> FileList.cancelUploadMsg CancelUpload
        |> FileList.setListStateMsg SetListState
        |> FileList.defaultSort (FileList.SortByCustom UploadedOn)
        |> FileList.defaultSortDirection FileList.Desc
        |> FileList.taggable
            [ "SomeTag"
            , "AnotherTag"
            ]
        |> FileList.column
            { id = UploadedOn
            , label = "Uploaded on"
            , html = .uploadedAt >> Date.Extra.toFormattedString "d MMM YYY HH:mm" >> text
            , sorter = \a b -> Date.Extra.compare a.uploadedAt b.uploadedAt
            }
        |> FileList.column
            { id = UploadedBy
            , label = "Uploaded by"
            , html = .uploadedBy >> text
            , sorter = \a b -> compare a.uploadedBy b.uploadedBy
            }


view : Model -> Html Msg
view { upload, files, list } =
    div
        [ style
            [ ( "width", "700px" )
            , ( "border", "1px solid #000" )
            ]
        ]
        [ Upload.view upload uploadConfig
        , hr [] []
        , FileList.view list fileListConfig upload files
        ]



---- SUBSCRIPTIONS ----


base64EncodeFileSub : Upload.State file -> Sub (Result String ( UploadId, Upload.UploadingFile file ))
base64EncodeFileSub upload =
    Upload.fileContentRead (Decode.decodeValue <| Upload.base64PortDecoder upload)


fileUploadedSub : Sub Msg
fileUploadedSub =
    Upload.uploaded (Decode.decodeValue UploadId.decoder >> UploadedFile)


fileUploadProgressSub : Sub Msg
fileUploadProgressSub =
    Upload.uploadProgress
        (\( id, progress ) ->
            case Decode.decodeValue UploadId.decoder id of
                Ok uploadId ->
                    UploadProgress uploadId progress

                Err _ ->
                    NoOp
        )


subscriptions : Model -> Sub Msg
subscriptions { upload } =
    Sub.batch
        [ Sub.map Base64EncodedFile (base64EncodeFileSub upload)
        , fileUploadedSub
        , fileUploadProgressSub
        ]
