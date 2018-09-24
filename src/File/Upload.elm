port module File.Upload
    exposing
        ( Config
        , State
        , UploadingFile
        , base64EncodeFiles
        , base64EncodedData
        , base64PortDecoder
        , browseClick
        , browseFiles
        , cancelUpload
        , config
        , drag
        , dropActive
        , fileContentRead
        , fileName
        , fileReadSuccess
        , fileUploadSuccess
        , init
        , inputId
        , isImage
        , maximumFileSize
        , onChangeFiles
        , signedUrlMetadataEncoder
        , updateS3UploadProgress
        , uploadCancelled
        , uploadFileToSignedUrl
        , uploadPercentage
        , uploadProgress
        , uploaded
        , uploads
        , view
        )

--import File.File as File

import Drag
import File.Base64Encoded as Base64Encoded exposing (Base64Encoded)
import File.SignedUrl as SignedUrl exposing (SignedUrl)
import File.UploadId as UploadId exposing (UploadId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onWithOptions)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


--- PORTS -----


port browseClick : String -> Cmd msg


port uploadProgress : (( Encode.Value, Float ) -> msg) -> Sub msg


port uploadCancelled : Encode.Value -> Cmd msg


port fileContentRead : (Encode.Value -> msg) -> Sub msg


port readFileContent : ( Encode.Value, Decode.Value ) -> Cmd msg


port upload : ( Encode.Value, Encode.Value, Encode.Value ) -> Cmd msg


port uploaded : (Encode.Value -> msg) -> Sub msg



---- STATE ----


type UploadingFile file
    = UploadingFile Drag.File (UploadStatus file)


type UploadStatus file
    = ReadingBase64
    | GettingSignedS3Url Base64Encoded
    | UploadingToS3 Base64Encoded SignedUrl Float file


type State file
    = State (StateRec file)


type alias StateRec file =
    { dropActive : Bool
    , selectAllToggled : Bool
    , uploads : UploadId.Collection (UploadingFile file)
    }


type Config msg
    = Config (ConfigRec msg)


type alias ConfigRec msg =
    { onChangeFilesMsg : String -> List Drag.File -> msg
    , softDeleteSelectedMsg : msg
    , browseClickMsg : String -> msg
    , dragOverMsg : Drag.Event -> msg
    , dragLeaveMsg : Drag.Event -> msg
    , dropMsg : Drag.Event -> msg
    , maximumFileSize : Int
    , inputId : String
    }


base64Success : ( UploadId, String ) -> State file -> State file
base64Success ( uploadId, base64 ) (State state) =
    State state


init : State file
init =
    State <|
        { dropActive = False
        , selectAllToggled = False
        , uploads = UploadId.init
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
        , maximumFileSize = 5000
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


uploads : State file -> UploadId.Collection (UploadingFile file)
uploads (State { uploads }) =
    uploads


fileName : UploadingFile file -> String
fileName (UploadingFile { name } _) =
    name


isImage : UploadingFile file -> Bool
isImage (UploadingFile { typeMIME } _) =
    String.startsWith "image" typeMIME


uploadPercentage : UploadingFile file -> Float
uploadPercentage file =
    case file of
        UploadingFile _ (UploadingToS3 _ _ percentage _) ->
            percentage

        _ ->
            0.0


base64EncodedData : UploadingFile file -> Maybe Base64Encoded
base64EncodedData (UploadingFile file status) =
    case status of
        ReadingBase64 ->
            Nothing

        GettingSignedS3Url base64Encoded ->
            Just base64Encoded

        UploadingToS3 base64Encoded _ _ _ ->
            Just base64Encoded



---- UPDATE ----


dropActive : Bool -> State file -> State file
dropActive isActive (State state) =
    State { state | dropActive = isActive }


base64EncodeFiles : List Drag.File -> State file -> ( State file, Cmd msg )
base64EncodeFiles files (State state) =
    let
        ( updatedUploadCollection, insertedIds ) =
            files
                |> List.map (\file -> UploadingFile file ReadingBase64)
                |> List.foldl
                    (\file ( uploadsCollection, insertedIds ) ->
                        let
                            ( id, collection ) =
                                UploadId.insert file uploadsCollection
                        in
                        ( collection
                        , id :: insertedIds
                        )
                    )
                    ( state.uploads, [] )
    in
    ( State
        { state
            | uploads = updatedUploadCollection
        }
    , readCmds insertedIds updatedUploadCollection
    )


readCmds : List UploadId -> UploadId.Collection (UploadingFile file) -> Cmd msg
readCmds uploadIds collection =
    uploadIds
        |> Debug.log "ids"
        |> List.filterMap
            (\id ->
                collection
                    |> UploadId.get id
                    |> Maybe.map
                        (\(UploadingFile { data } _) ->
                            readFileContent ( UploadId.encoder id, data )
                        )
            )
        |> Debug.log "cmds"
        |> Cmd.batch


fileReadSuccess : UploadId -> UploadingFile file -> State file -> State file
fileReadSuccess uploadId file (State state) =
    State <|
        { state | uploads = UploadId.update uploadId (always (Just file)) state.uploads }


fileUploadSuccess : UploadId -> State file -> ( State file, Maybe file )
fileUploadSuccess requestId (State state) =
    let
        maybeFile =
            state.uploads
                |> UploadId.get requestId
                |> Maybe.andThen
                    (\file ->
                        case file of
                            UploadingFile _ (UploadingToS3 _ _ _ file) ->
                                Just file

                            _ ->
                                Nothing
                    )

        uploads =
            UploadId.remove requestId state.uploads
    in
    ( State { state | uploads = uploads }
    , maybeFile
    )


uploadFileToSignedUrl : SignedUrl -> file -> UploadId -> State file -> ( State file, Cmd msg )
uploadFileToSignedUrl signedUrl backendFile uploadId (State state) =
    let
        ( uploads, cmd ) =
            case UploadId.get uploadId state.uploads of
                Just (UploadingFile rawFile (GettingSignedS3Url base64)) ->
                    let
                        uploadingFile =
                            UploadingFile rawFile (UploadingToS3 base64 signedUrl 0.0 backendFile)
                    in
                    ( UploadId.update uploadId (always (Just uploadingFile)) state.uploads
                    , uploadCmds [ ( uploadId, uploadingFile ) ]
                    )

                _ ->
                    ( state.uploads
                    , Cmd.none
                    )
    in
    ( State <|
        { state | uploads = uploads }
    , cmd
    )


uploadCmds : List ( UploadId, UploadingFile file ) -> Cmd msg
uploadCmds files =
    files
        |> List.map
            (\( id, uploadingFile ) ->
                case uploadingFile of
                    UploadingFile _ (UploadingToS3 base64 signedUrl _ _) ->
                        upload
                            ( UploadId.encoder id
                            , SignedUrl.encoder signedUrl
                            , Base64Encoded.encoder base64
                            )

                    _ ->
                        Cmd.none
            )
        |> Cmd.batch


updateS3UploadProgress : UploadId -> Float -> State file -> State file
updateS3UploadProgress id progress (State state) =
    State <|
        { state
            | uploads =
                UploadId.update id
                    (Maybe.map
                        (\upload ->
                            case upload of
                                UploadingFile rawFile (UploadingToS3 base64 signedUrl _ backendFile) ->
                                    UploadingFile rawFile (UploadingToS3 base64 signedUrl progress backendFile)

                                _ ->
                                    upload
                        )
                    )
                    state.uploads
        }


cancelUpload : UploadId -> State file -> ( State file, Cmd msg )
cancelUpload uploadId (State state) =
    ( State { state | uploads = UploadId.remove uploadId state.uploads }
    , uploadCancelled (UploadId.encoder uploadId)
    )



---- VIEW ----


view : State file -> Config msg -> Html msg
view (State state) (Config config) =
    div []
        [ dropZone state config
        , fileInput config
        ]


dropZone : StateRec file -> ConfigRec msg -> Html msg
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



---- ENCODER ----


signedUrlMetadataEncoder : UploadingFile file -> Encode.Value
signedUrlMetadataEncoder (UploadingFile { typeMIME, name, size } _) =
    Encode.object
        [ ( "contentType", Encode.string typeMIME )
        , ( "fileName", Encode.string name )
        , ( "size", Encode.int size )
        ]


base64PortDecoder : State file -> Decode.Decoder ( UploadId, UploadingFile file )
base64PortDecoder (State { uploads }) =
    Decode.field "id" UploadId.decoder
        |> Decode.andThen
            (\requestId ->
                case UploadId.get requestId uploads of
                    Just (UploadingFile rawFile _) ->
                        Pipeline.decode GettingSignedS3Url
                            |> Pipeline.required "result" Base64Encoded.decoder
                            |> Decode.andThen (UploadingFile rawFile >> (,) requestId >> Decode.succeed)

                    _ ->
                        Decode.fail "Can't find request"
            )
