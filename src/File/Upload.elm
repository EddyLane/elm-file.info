port module File.Upload
    exposing
        ( Config
        , State
        , base64EncodeFiles
        , browseClick
        , browseFiles
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
        , maximumFileSize
        , nameFn
        , onChangeFiles
        , thumbnailSrc
        , thumbnailSrcFn
        , updateS3UploadProgress
        , uploadFile
        , uploadFileToSignedUrl
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



---- STATE ----


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
    , dropMsg : Drag.Event -> msg
    , maximumFileSize : Int
    , inputId : String
    , nameFn : file -> String
    , contentTypeFn : file -> String
    , thumbnailSrcFn : file -> String
    }


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


files : State file -> List file -> List (File.UploadState file)
files (State { reading, signing, uploading }) uploaded =
    List.concat
        [ List.map File.lifeCycleReading reading
        , List.map File.lifeCycleSigning signing
        , List.map File.lifeCycleUploading uploading
        , List.map File.lifeCycleUploaded uploaded
        ]


fileName : Config msg file -> File.UploadState file -> String
fileName (Config { nameFn }) file =
    File.name nameFn file


thumbnailSrc : Config msg file -> File.UploadState file -> String
thumbnailSrc (Config { thumbnailSrcFn, contentTypeFn }) file =
    File.thumbnailSrc thumbnailSrcFn contentTypeFn file



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
            | reading = File.removeReadRequest file state.reading
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
            File.signed file signedUrl 40 backendFile
    in
    ( State <|
        { state
            | signing = File.removeSigningRequest signedFile state.signing
            , uploading = signedFile :: state.uploading
        }
    , File.uploadCmds [ signedFile ]
    )


updateS3UploadProgress : Int -> Float -> State file -> State file
updateS3UploadProgress id progress (State state) =
    State <|
        { state | uploading = File.updateUploadProgress id progress state.uploading }



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
