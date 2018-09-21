port module File.Upload
    exposing
        ( Config
        , State
        , UploadState
        , base64EncodeFiles
        , browseClick
        , browseFiles
        , cancelTrigger
        , cancelUpload
        , cancelUploadMsg
        , config
        , contentTypeFn
        , drag
        , dropActive
        , fileName
        , fileReadSuccess
        , fileUploadSuccess
        , files
        , getReading
        , init
        , inputId
        , lifeCycleReading
        , lifeCycleSigning
        , lifeCycleUploaded
        , lifeCycleUploading
        , maximumFileSize
        , nameFn
        , onChangeFiles
        , thumbnailSrc
        , thumbnailSrcFn
        , updateS3UploadProgress
        , uploadCancelled
        , uploadFile
        , uploadFileToSignedUrl
        , uploadPercentage
        , uploadProgress
        , view
        )

import Drag
import File.File as File
import File.SignedUrl as SignedUrl exposing (SignedUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onWithOptions)
import Json.Decode as Decode
import Json.Encode as Encode
import Mouse


--- PORTS -----


port browseClick : String -> Cmd msg


port uploadProgress : (( Int, Float ) -> msg) -> Sub msg


port uploadCancelled : Int -> Cmd msg



---- STATE ----


type UploadState file
    = ReadingBase64 File.FileReadPortRequest
    | GettingSignedS3Url File.FileReadPortResponse
    | UploadingToS3 (File.FileSigned file)
    | Uploaded file


type State file
    = State (StateRec file)


type alias StateRec file =
    { dropActive : Bool
    , selectAllToggled : Bool
    , requestId : Int
    , reading : List File.FileReadPortRequest
    , signing : List File.FileReadPortResponse
    , uploading : List (File.FileSigned file)
    }


type Config msg file
    = Config (ConfigRec msg file)


type alias ConfigRec msg file =
    { onChangeFilesMsg : String -> List Drag.File -> msg
    , uploadFileMsg : File.FileReadPortRequest -> msg
    , softDeleteSelectedMsg : msg
    , browseClickMsg : String -> msg
    , dragOverMsg : Drag.Event -> msg
    , dragLeaveMsg : Drag.Event -> msg
    , cancelUploadMsg : UploadState file -> msg
    , dropMsg : Drag.Event -> msg
    , maximumFileSize : Int
    , inputId : String
    , nameFn : file -> String
    , contentTypeFn : file -> String
    , thumbnailSrcFn : file -> String
    }


lifeCycleReading : File.FileReadPortRequest -> UploadState file
lifeCycleReading =
    ReadingBase64


lifeCycleSigning : File.FileReadPortResponse -> UploadState file
lifeCycleSigning =
    GettingSignedS3Url


lifeCycleUploading : File.FileSigned file -> UploadState file
lifeCycleUploading =
    UploadingToS3


lifeCycleUploaded : file -> UploadState file
lifeCycleUploaded =
    Uploaded


init : State file
init =
    State <|
        { dropActive = False
        , selectAllToggled = False
        , requestId = 0
        , reading = []
        , signing = []
        , uploading = []
        }


config : msg -> Config msg file
config noOpMsg =
    Config <|
        { onChangeFilesMsg = always (always noOpMsg)
        , softDeleteSelectedMsg = noOpMsg
        , browseClickMsg = always noOpMsg
        , cancelUploadMsg = always noOpMsg
        , dragOverMsg = always noOpMsg
        , dragLeaveMsg = always noOpMsg
        , dropMsg = always noOpMsg
        , uploadFileMsg = always noOpMsg
        , maximumFileSize = 5000
        , inputId = "elm-file-upload-input"
        , nameFn = always "-"
        , contentTypeFn = always "-"
        , thumbnailSrcFn = always ""
        }


onChangeFiles : (String -> List Drag.File -> msg) -> Config msg file -> Config msg file
onChangeFiles msg (Config configRec) =
    Config <|
        { configRec | onChangeFilesMsg = msg }


inputId : String -> Config msg file -> Config msg file
inputId inputId (Config configRec) =
    Config <|
        { configRec | inputId = inputId }


softDelete : msg -> Config msg file -> Config msg file
softDelete msg (Config configRec) =
    Config <|
        { configRec | softDeleteSelectedMsg = msg }


browseFiles : (String -> msg) -> Config msg file -> Config msg file
browseFiles msg (Config configRec) =
    Config <|
        { configRec | browseClickMsg = msg }


cancelUploadMsg : (UploadState file -> msg) -> Config msg file -> Config msg file
cancelUploadMsg msg (Config configRec) =
    Config <|
        { configRec | cancelUploadMsg = msg }


drag : (Drag.Event -> msg) -> (Drag.Event -> msg) -> (Drag.Event -> msg) -> Config msg file -> Config msg file
drag over leave drop (Config configRec) =
    Config <|
        { configRec
            | dragOverMsg = over
            , dragLeaveMsg = leave
            , dropMsg = drop
        }


nameFn : (file -> String) -> Config msg file -> Config msg file
nameFn fn (Config configRec) =
    Config <|
        { configRec | nameFn = fn }


contentTypeFn : (file -> String) -> Config msg file -> Config msg file
contentTypeFn fn (Config configRec) =
    Config <|
        { configRec | contentTypeFn = fn }


thumbnailSrcFn : (file -> String) -> Config msg file -> Config msg file
thumbnailSrcFn fn (Config configRec) =
    Config <|
        { configRec | thumbnailSrcFn = fn }


maximumFileSize : Int -> Config msg file -> Config msg file
maximumFileSize size (Config configRec) =
    Config <|
        { configRec | maximumFileSize = size }


uploadFile : (File.FileReadPortRequest -> msg) -> Config msg file -> Config msg file
uploadFile msg (Config configRec) =
    Config <|
        { configRec | uploadFileMsg = msg }


getReading : State file -> List File.FileReadPortRequest
getReading (State { reading }) =
    reading


files : State file -> List file -> List (UploadState file)
files (State { reading, signing, uploading }) uploaded =
    List.concat
        [ List.map lifeCycleReading reading
        , List.map lifeCycleSigning signing
        , List.map lifeCycleUploading uploading
        , List.map lifeCycleUploaded uploaded
        ]


fileName : Config msg file -> UploadState file -> String
fileName (Config { nameFn }) file =
    case file of
        ReadingBase64 request ->
            .name <| File.fileFromRequest request

        GettingSignedS3Url response ->
            .name <| File.fileFromResponse response

        UploadingToS3 signed ->
            .name <| File.fileFromSigned signed

        Uploaded uploaded ->
            nameFn uploaded


thumbnailSrc : Config msg file -> UploadState file -> String
thumbnailSrc (Config { thumbnailSrcFn, contentTypeFn }) file =
    case ( isImage contentTypeFn file, file ) of
        ( True, GettingSignedS3Url response ) ->
            File.base64EncodedSigning response

        ( True, UploadingToS3 response ) ->
            File.base64EncodedUploading response

        ( True, Uploaded file ) ->
            thumbnailSrcFn file

        _ ->
            ""


name : (file -> String) -> UploadState file -> String
name uploadedFn file =
    case file of
        ReadingBase64 request ->
            .name <| File.fileFromRequest request

        GettingSignedS3Url response ->
            .name <| File.fileFromResponse response

        UploadingToS3 signed ->
            .name <| File.fileFromSigned signed

        Uploaded uploaded ->
            uploadedFn uploaded


uploadPercentage : UploadState file -> Float
uploadPercentage file =
    case file of
        ReadingBase64 _ ->
            0.0

        GettingSignedS3Url _ ->
            10.0

        UploadingToS3 file ->
            File.progress file - 10

        Uploaded _ ->
            100.0


isImage : (file -> String) -> UploadState file -> Bool
isImage contentTypeFn file =
    case file of
        ReadingBase64 request ->
            File.isImageReading request

        GettingSignedS3Url request ->
            File.isImageSigning request

        UploadingToS3 request ->
            File.isImageUploading request

        Uploaded backendFile ->
            String.startsWith "image" (contentTypeFn backendFile)



---- UPDATE ----


dropActive : Bool -> State file -> State file
dropActive isActive (State state) =
    State { state | dropActive = isActive }


base64EncodeFiles : List Drag.File -> State file -> ( State file, Cmd msg )
base64EncodeFiles files (State state) =
    let
        reading =
            state.reading ++ File.requests (state.requestId + 1) files
    in
    ( State
        { state
            | reading = reading
            , requestId = state.requestId + List.length reading
        }
    , File.readCmds reading
    )


fileReadSuccess : File.FileReadPortResponse -> State file -> State file
fileReadSuccess file (State state) =
    State <|
        { state
            | reading = File.fileReadSuccess file state.reading
            , signing = file :: state.signing
        }


fileUploadSuccess : Int -> State file -> ( State file, Maybe file )
fileUploadSuccess requestId (State state) =
    let
        ( uploading, maybeBackendFile ) =
            File.popUploadingRequest requestId state.uploading
    in
    ( State { state | uploading = uploading }
    , maybeBackendFile
    )


uploadFileToSignedUrl : SignedUrl -> file -> File.FileReadPortResponse -> State file -> ( State file, Cmd msg )
uploadFileToSignedUrl signedUrl backendFile file (State state) =
    let
        signedFile =
            File.signed file signedUrl 10 backendFile
    in
    ( State <|
        { state
            | signing = File.fileSigningSuccess signedFile state.signing
            , uploading = signedFile :: state.uploading
        }
    , File.uploadCmds [ signedFile ]
    )


updateS3UploadProgress : Int -> Float -> State file -> State file
updateS3UploadProgress id progress (State state) =
    State <|
        { state | uploading = File.updateUploadProgress id progress state.uploading }


cancelTrigger : Config msg file -> UploadState file -> msg
cancelTrigger (Config { cancelUploadMsg }) file =
    cancelUploadMsg file


cancelUpload : UploadState file -> State file -> ( State file, Cmd msg )
cancelUpload file (State state) =
    case file of
        ReadingBase64 request ->
            ( State { state | reading = File.removeReadRequest request state.reading }
            , Cmd.none
            )

        GettingSignedS3Url response ->
            ( State { state | signing = File.removeSigningRequest response state.signing }
            , uploadCancelled (File.idFromResponse response)
            )

        UploadingToS3 signed ->
            ( State { state | uploading = File.removeUploadingRequest signed state.uploading }
            , uploadCancelled (File.idFromSigned signed)
            )

        _ ->
            ( State state, Cmd.none )



--
--        UploadingToS3 request ->
--            { state
--                | uploading =
--                    List.filter
--                        (\(FileSigned (FileReadPortResponse (FileReadPortRequest id _) _) _ _ _) ->
--                            id /= requestId
--                        )
--                        state.uploading
--            }
---- VIEW ----


view : State file -> Config msg file -> Html msg
view (State state) (Config config) =
    div []
        [ dropZone state config
        , fileInput config
        ]


dropZone : StateRec file -> ConfigRec msg file -> Html msg
dropZone state config =
    div
        [ style
            [ ( "width", "100%" )
            , ( "height", "150px" )
            , ( "border-bottom"
              , if state.dropActive then
                    "2px dashed #ddd"
                else
                    "2px dashed transparent"
              )
            , ( "background"
              , if state.dropActive then
                    "#dff0d8"
                else
                    "#f7f7f7"
              )
            ]
        , Drag.onOver config.dragOverMsg
        , Drag.onLeave config.dragLeaveMsg
        , Drag.onDrop config.dropMsg
        ]
    <|
        [ div [] [ text "Files" ]
        , p [ class "upload-file-container" ]
            [ i [ class "fa fa-upload" ] []
            , text "Drop your files here or "
            , a
                [ href "javascript:void(0)"
                , onClick (config.browseClickMsg config.inputId)
                ]
                [ text "browse for a file" ]
            , text " to upload."
            ]
        ]


fileInput : ConfigRec msg file -> Html msg
fileInput { inputId, onChangeFilesMsg } =
    input
        [ style [ ( "display", "none" ) ]
        , attribute "multiple" ""
        , type_ "file"
        , id inputId
        , onWithOptions
            "change"
            { stopPropagation = False
            , preventDefault = False
            }
            (fileInputDecoder inputId onChangeFilesMsg)
        ]
        []


fileInputDecoder : String -> (String -> List Drag.File -> msg) -> Decode.Decoder msg
fileInputDecoder inputId msg =
    Drag.fileDecoder
        |> Drag.fileListDecoder
        |> Decode.at [ "target", "files" ]
        |> Decode.map (msg inputId)
