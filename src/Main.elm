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
    Upload.State Attachment


init : ( Model, Cmd Msg )
init =
    ( Upload.init
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
    case msg of
        DragFilesOver _ ->
            ( Upload.dropActive True model
            , Cmd.none
            )

        DragFilesLeave _ ->
            ( Upload.dropActive False model
            , Cmd.none
            )

        OpenFileBrowser inputID ->
            ( model
            , Upload.browseClick inputID
            )

        DropFiles { dataTransfer } ->
            model
                |> Upload.dropActive False
                |> Upload.base64EncodeFiles dataTransfer.files

        InputFiles _ files ->
            Upload.base64EncodeFiles files model

        Base64EncodeFile (Ok file) ->
            ( Upload.fileReadSuccess file model
            , Task.attempt (GetSignedS3Url file) getSignedUrl
            )

        Base64EncodeFile (Err err) ->
            ( model
            , Cmd.none
            )

        GetSignedS3Url file (Ok { attachment, signedUrl }) ->
            Upload.uploadFileToSignedUrl signedUrl attachment file model

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


base64EncodeFileSub : List File.FileReadPortRequest -> Sub (Result String File.FileReadPortResponse)
base64EncodeFileSub requests =
    File.fileContentRead (Decode.decodeValue <| File.filePortDecoder requests)



--uploadFileSub :


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Base64EncodeFile (base64EncodeFileSub <| Upload.getReading model) ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
