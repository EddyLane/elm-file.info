port module File.Upload
    exposing
        ( Config
        , State
        , addFileReadRequests
        , backendUrl
        , browseClick
        , browseFiles
        , config
        , drag
        , dropActive
        , fileReadSuccess
        , files
        , getReading
        , getSignedS3UrlSuccess
        , init
        , inputId
        , maximumFileSize
        , onChangeFiles
        , uploadFile
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



---- STATE ----


type State
    = State StateRec


type alias StateRec =
    { dropActive : Bool
    , selectAllToggled : Bool
    , requestId : Int
    , reading : List File.FileReadPortRequest
    , signing : List File.FileReadPortResponse
    , uploading : List File.FileSigned
    }


type Config msg
    = Config (ConfigRec msg)


type alias ConfigRec msg =
    { onChangeFilesMsg : String -> List Drag.File -> msg
    , uploadFileMsg : File.FileReadPortRequest -> msg
    , softDeleteSelectedMsg : msg
    , browseClickMsg : String -> msg
    , dragOverMsg : Drag.Event -> msg
    , dragLeaveMsg : Drag.Event -> msg
    , dropMsg : Drag.Event -> msg
    , maximumFileSize : Int
    , backendUrl : String
    , inputId : String
    }


init : State
init =
    State <|
        { dropActive = False
        , selectAllToggled = False
        , requestId = 0
        , reading = []
        , signing = []
        , uploading = []
        }


config : msg -> Config msg
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
        , backendUrl = ""
        , inputId = "elm-file-upload-input"
        }


onChangeFiles : (String -> List Drag.File -> msg) -> Config msg -> Config msg
onChangeFiles msg (Config configRec) =
    Config <|
        { configRec | onChangeFilesMsg = msg }


inputId : String -> Config msg -> Config msg
inputId inputId (Config configRec) =
    Config <|
        { configRec | inputId = inputId }


softDelete : msg -> Config msg -> Config msg
softDelete msg (Config configRec) =
    Config <|
        { configRec | softDeleteSelectedMsg = msg }


browseFiles : (String -> msg) -> Config msg -> Config msg
browseFiles msg (Config configRec) =
    Config <|
        { configRec | browseClickMsg = msg }


drag : (Drag.Event -> msg) -> (Drag.Event -> msg) -> (Drag.Event -> msg) -> Config msg -> Config msg
drag over leave drop (Config configRec) =
    Config <|
        { configRec
            | dragOverMsg = over
            , dragLeaveMsg = leave
            , dropMsg = drop
        }


maximumFileSize : Int -> Config msg -> Config msg
maximumFileSize size (Config configRec) =
    Config <|
        { configRec | maximumFileSize = size }


uploadFile : (File.FileReadPortRequest -> msg) -> Config msg -> Config msg
uploadFile msg (Config configRec) =
    Config <|
        { configRec | uploadFileMsg = msg }


backendUrl : String -> Config msg -> Config msg
backendUrl backendUrl (Config configRec) =
    Config <|
        { configRec | backendUrl = backendUrl }


getReading : State -> List File.FileReadPortRequest
getReading (State { reading }) =
    reading


files : State -> List File.FileLifecycle
files (State { reading, signing, uploading }) =
    List.concat
        [ List.map File.lifeCycleReading reading
        , List.map File.lifeCycleSigning signing
        , List.map File.lifeCycleUploading uploading
        ]



---- UPDATE ----


dropActive : Bool -> State -> State
dropActive isActive (State state) =
    State { state | dropActive = isActive }


addFileReadRequests : List Drag.File -> State -> ( State, Cmd msg )
addFileReadRequests files (State state) =
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


fileReadSuccess : File.FileReadPortResponse -> State -> State
fileReadSuccess file (State state) =
    State <|
        { state
            | reading = File.removeReadRequest file state.reading
            , signing = file :: state.signing
        }


getSignedS3UrlSuccess : SignedUrl -> File.FileReadPortResponse -> State -> ( State, Cmd msg )
getSignedS3UrlSuccess signedUrl file (State state) =
    let
        signedFile =
            File.signed file signedUrl
    in
    ( State <|
        { state
            | signing = File.removeSigningRequest signedFile state.signing
            , uploading = signedFile :: state.uploading
        }
    , File.uploadCmds [ signedFile ]
    )



---- VIEW ----


view : State -> Config msg -> Html msg
view (State state) (Config config) =
    div []
        [ dropZone state config
        , fileInput config
        ]


dropZone : StateRec -> ConfigRec msg -> Html msg
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


fileInput : ConfigRec msg -> Html msg
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
