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
    }


attachmentDecoder : Decode.Decoder Attachment
attachmentDecoder =
    Pipeline.decode Attachment
        |> Pipeline.required "reference" Decode.string



---- UPDATE ----


type Msg
    = NoOp
    | OpenFileBrowser String
    | InputFiles String (List Drag.File)
    | Base64EncodeFile (Result String File.FileReadPortResponse)
    | DragFilesOver Drag.Event
    | DragFilesLeave Drag.Event
    | DropFiles Drag.Event
    | GetSignedS3Url File.FileReadPortResponse (Result Http.Error AttachmentResponse)


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

        Base64EncodeFile (Ok file) ->
            ( { model | upload = Upload.fileReadSuccess file model.upload }
            , Task.attempt (GetSignedS3Url file) getSignedUrl
            )

        Base64EncodeFile (Err err) ->
            ( model
            , Cmd.none
            )

        GetSignedS3Url file (Ok { attachment, signedUrl }) ->
            let
                ( upload, uploadCmd ) =
                    Upload.uploadFileToSignedUrl signedUrl attachment file model.upload
            in
            ( { model | upload = upload }
            , uploadCmd
            )

        GetSignedS3Url _ (Err e) ->
            ( model
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


getSignedUrl : Task Http.Error AttachmentResponse
getSignedUrl =
    Http.get signedUrlProviderUrl
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


view : Model -> Html Msg
view { upload } =
    div
        [ style
            [ ( "width", "700px" )
            , ( "border", "1px solid #000" )
            ]
        ]
        [ Upload.view upload uploadConfig
        , hr [] []
        , FileList.view upload
        ]



---- SUBSCRIPTIONS ----


base64EncodeFileSub : List File.FileReadPortRequest -> Sub (Result String File.FileReadPortResponse)
base64EncodeFileSub requests =
    File.fileContentRead (Decode.decodeValue <| File.filePortDecoder requests)


subscriptions : Model -> Sub Msg
subscriptions { upload } =
    Sub.map Base64EncodeFile (base64EncodeFileSub <| Upload.getReading upload)
