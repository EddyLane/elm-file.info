module Page.Demo exposing (Model, Msg, init, subscriptions, update, view)

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


getAttachmentsUrl : String
getAttachmentsUrl =
    "http://localhost:3003/get-attachments"



---- MODEL ----


type alias Model =
    { upload : Upload.State Attachment
    , files : List Attachment
    }


init : Task Http.Error Model
init =
    Task.map (Model Upload.init) loadAttachments


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
    { reference : String
    , contentType : String
    , fileName : String
    }


attachmentDecoder : Decode.Decoder Attachment
attachmentDecoder =
    Pipeline.decode Attachment
        |> Pipeline.required "reference" Decode.string
        |> Pipeline.required "contentType" Decode.string
        |> Pipeline.required "fileName" Decode.string



--        |> Pipeline.required "contentType" Decode.string
---- UPDATE ----


type Msg
    = NoOp
    | OpenFileBrowser String
    | InputFiles String (List Drag.File)
    | Base64EncodedFile (Result String File.FileReadPortResponse)
    | DragFilesOver Drag.Event
    | DragFilesLeave Drag.Event
    | DropFiles Drag.Event
    | GotSignedS3Url File.FileReadPortResponse (Result Http.Error AttachmentResponse)
    | UploadedFile (Result String Int)
    | UploadProgress Int Float
    | CancelUpload (Upload.UploadState Attachment)


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

        Base64EncodedFile (Ok file) ->
            ( { model | upload = Upload.fileReadSuccess file model.upload }
            , Task.attempt (GotSignedS3Url file) (getSignedUrl file)
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

        NoOp ->
            ( model
            , Cmd.none
            )


getSignedUrl : File.FileReadPortResponse -> Task Http.Error AttachmentResponse
getSignedUrl file =
    Http.post signedUrlProviderUrl
        (Http.jsonBody <| File.signedUrlMetadataEncoder file)
        (Pipeline.decode AttachmentResponse
            |> Pipeline.required "attachment" attachmentDecoder
            |> Pipeline.required "signedUrl" SignedUrl.decoder
        )
        |> Http.toTask



--deleteAttachment :
--downloadFile : Attachment
---- VIEW ----


uploadConfig : Upload.Config Msg Attachment
uploadConfig =
    Upload.config NoOp
        |> Upload.maximumFileSize 500
        |> Upload.onChangeFiles InputFiles
        |> Upload.browseFiles OpenFileBrowser
        |> Upload.drag DragFilesOver DragFilesLeave DropFiles
        |> Upload.cancelUploadMsg CancelUpload
        |> Upload.inputId "elm-file-example"
        |> Upload.nameFn .fileName
        |> Upload.contentTypeFn .contentType
        |> Upload.thumbnailSrcFn (\attachment -> "http://localhost:3003/download/" ++ attachment.reference)


view : Model -> Html Msg
view { upload, files } =
    div
        [ style
            [ ( "width", "700px" )
            , ( "border", "1px solid #000" )
            ]
        ]
        [ Upload.view upload uploadConfig
        , hr [] []
        , FileList.view uploadConfig upload files
        ]



---- SUBSCRIPTIONS ----


base64EncodeFileSub : List File.FileReadPortRequest -> Sub (Result String File.FileReadPortResponse)
base64EncodeFileSub requests =
    File.fileContentRead (Decode.decodeValue <| File.base64PortDecoder requests)


fileUploadedSub : Sub Msg
fileUploadedSub =
    File.uploaded (Ok >> UploadedFile)


fileUploadProgressSub : Sub Msg
fileUploadProgressSub =
    Upload.uploadProgress (\( id, progress ) -> UploadProgress id progress)


subscriptions : Model -> Sub Msg
subscriptions { upload } =
    Sub.batch
        [ Sub.map Base64EncodedFile (base64EncodeFileSub <| Upload.getReading upload)
        , fileUploadedSub
        , fileUploadProgressSub
        ]
