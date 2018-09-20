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


getAttachmentsUrl : String
getAttachmentsUrl =
    "http://localhost:3003/attachments"



---- MODEL ----


type Model
    = Loaded (PageState Attachment)
    | Loading


type alias PageState file =
    { files : List Attachment
    , upload : Upload.State file
    }


init : ( Model, Cmd Msg )
init =
    ( Loading
    , Cmd.none
    )



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
    case model of
        Loading ->
            ( model, Cmd.none )

        Loaded pageState ->
            updatePage msg pageState
                |> Tuple.mapFirst Loaded


updatePage : Msg -> PageState Attachment -> ( PageState Attachment, Cmd Msg )
updatePage msg pageState =
    case msg of
        DragFilesOver _ ->
            ( { pageState | upload = Upload.dropActive True pageState.upload }
            , Cmd.none
            )

        DragFilesLeave _ ->
            ( { pageState | upload = Upload.dropActive False pageState.upload }
            , Cmd.none
            )

        OpenFileBrowser inputID ->
            ( pageState
            , Upload.browseClick inputID
            )

        DropFiles { dataTransfer } ->
            let
                ( upload, base64Cmd ) =
                    pageState.upload
                        |> Upload.dropActive False
                        |> Upload.base64EncodeFiles dataTransfer.files
            in
            ( { pageState | upload = upload }
            , base64Cmd
            )

        InputFiles _ files ->
            let
                ( upload, base64Cmd ) =
                    pageState.upload
                        |> Upload.dropActive False
                        |> Upload.base64EncodeFiles files
            in
            ( { pageState | upload = upload }
            , base64Cmd
            )

        Base64EncodeFile (Ok file) ->
            ( { pageState | upload = Upload.fileReadSuccess file pageState.upload }
            , Task.attempt (GetSignedS3Url file) getSignedUrl
            )

        Base64EncodeFile (Err err) ->
            ( pageState
            , Cmd.none
            )

        GetSignedS3Url file (Ok { attachment, signedUrl }) ->
            let
                ( upload, uploadCmd ) =
                    Upload.uploadFileToSignedUrl signedUrl attachment file pageState.upload
            in
            ( { pageState | upload = upload }
            , uploadCmd
            )

        GetSignedS3Url _ (Err e) ->
            ( pageState
            , Cmd.none
            )

        NoOp ->
            ( pageState
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
view model =
    case model of
        Loaded { upload } ->
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

        Loading ->
            text "Loading..."



---- SUBSCRIPTIONS ----


base64EncodeFileSub : List File.FileReadPortRequest -> Sub (Result String File.FileReadPortResponse)
base64EncodeFileSub requests =
    File.fileContentRead (Decode.decodeValue <| File.filePortDecoder requests)



--uploadFileSub :


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loaded { upload } ->
            Sub.batch
                [ Sub.map Base64EncodeFile (base64EncodeFileSub <| Upload.getReading upload) ]

        Loading ->
            Sub.none



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
