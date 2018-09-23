port module File.Upload
    exposing
        ( Config
        , State
        , UploadState
        , UploadingFile
        , base64EncodeFiles
        , base64PortDecoder
        , browseClick
        , browseFiles
        , cancelTrigger
        , cancelUploadMsg
        , config
        , contentTypeFn
        , drag
        , dropActive
        , fileContentRead
          --        , lifeCycleReading
          --        , lifeCycleSigning
          --        , lifeCycleUploaded
          --        , lifeCycleUploading
        , fileName
        , fileReadSuccess
          --        , fileUploadSuccess
          --        , files
        , getReading
        , init
        , inputId
        , maximumFileSize
        , nameFn
        , onChangeFiles
        , signedUrlMetadataEncoder
          --        , cancelUpload
        , thumbnailSrc
        , thumbnailSrcFn
          --        , updateS3UploadProgressogress
        , uploadCancelled
          --        , uploadFile
        , uploadFileToSignedUrl
        , uploadPercentage
        , uploadProgress
        , uploaded
        , view
        )

--import File.File as File

import Drag
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


port upload : ( Encode.Value, String, String ) -> Cmd msg


port uploaded : (Encode.Value -> msg) -> Sub msg



---- STATE ----


type UploadState file
    = Uploading (UploadingFile file)
    | Uploaded file


type UploadingFile file
    = UploadingFile Drag.File (UploadStatus file)


type UploadStatus file
    = ReadingBase64
    | GettingSignedS3Url String
    | UploadingToS3 String SignedUrl Float file


type State file
    = State (StateRec file)


type alias StateRec file =
    { dropActive : Bool
    , selectAllToggled : Bool
    , requestId : UploadId
    , uploads : UploadId.Collection (UploadingFile file)
    }


type Config msg file
    = Config (ConfigRec msg file)


type alias ConfigRec msg file =
    { onChangeFilesMsg : String -> List Drag.File -> msg
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


base64Success : ( UploadId, String ) -> State file -> State file
base64Success ( uploadId, base64 ) (State state) =
    State state



--lifeCycleReading : File.FileReadPortRequest -> UploadState file
--lifeCycleReading =
--    ReadingBase64 >> Uploading
--
--
--lifeCycleSigning : File.FileReadPortResponse -> UploadState file
--lifeCycleSigning =
--    GettingSignedS3Url >> Uploading
--
--
--lifeCycleUploading : File.FileSigned file -> UploadState file
--lifeCycleUploading =
--    UploadingToS3 >> Uploading


lifeCycleUploaded : file -> UploadState file
lifeCycleUploaded =
    Uploaded


init : State file
init =
    State <|
        { dropActive = False
        , selectAllToggled = False
        , requestId = UploadId.init
        , uploads = UploadId.collection
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


getReading : State file -> List ( UploadId, Drag.File )
getReading (State { uploads }) =
    uploads
        |> UploadId.toList
        |> List.filterMap
            (\upload ->
                case upload of
                    ( uploadId, UploadingFile dragFile ReadingBase64 ) ->
                        Just ( uploadId, dragFile )

                    _ ->
                        Nothing
            )



--readingFiles : State file -> List File.FileReadPortRequest
--
--
--files : State file -> List file -> List (UploadState file)
--files (State { uploads }) uploaded =
--    List.concat
--        [ List.map lifeCycleReading reading
--        , List.map lifeCycleSigning signing
--        , List.map lifeCycleUploading uploading
--        , List.map lifeCycleUploaded uploaded
--        ]
--


fileName : Config msg file -> UploadState file -> String
fileName (Config { nameFn }) file =
    case file of
        Uploading (UploadingFile rawFile _) ->
            .name rawFile

        Uploaded uploaded ->
            nameFn uploaded


thumbnailSrc : Config msg file -> UploadState file -> String
thumbnailSrc (Config { thumbnailSrcFn, contentTypeFn }) file =
    case ( isImage contentTypeFn file, file ) of
        ( True, Uploading (UploadingFile _ (GettingSignedS3Url base64Encoded)) ) ->
            base64Encoded

        ( True, Uploading (UploadingFile _ (UploadingToS3 base64Encoded _ _ _)) ) ->
            base64Encoded

        ( True, Uploaded file ) ->
            thumbnailSrcFn file

        _ ->
            ""


uploadPercentage : UploadState file -> Float
uploadPercentage file =
    case file of
        Uploading (UploadingFile _ (UploadingToS3 _ _ percentage _)) ->
            percentage

        Uploaded _ ->
            100.0

        _ ->
            0.0


isImage : (file -> String) -> UploadState file -> Bool
isImage contentTypeFn file =
    case file of
        Uploading (UploadingFile { typeMIME } _) ->
            String.startsWith "image" typeMIME

        Uploaded backendFile ->
            String.startsWith "image" (contentTypeFn backendFile)



---- UPDATE ----


dropActive : Bool -> State file -> State file
dropActive isActive (State state) =
    State { state | dropActive = isActive }



--
--requests : UploadId -> List Drag.File -> List UploadingFile
--requests requestId =
--    List.indexedMap (\i file -> UploadingFile (UploadId.update i requestId) file)
--
--
--base64EncodeFiles : List Drag.File -> State file -> ( State file, Cmd msg )
--base64EncodeFiles files (State state) =
--    let
--        newRequests =
--            File.requests (UploadId.update 1 state.requestId) files
--
--        uploads =
--            files
--                |> List.indexedMap (,)
--                |> List.foldl
--                    (\acc ( index, file ) ->
--                        UploadId.insert (UploadId.update index state.requestId) file acc
--                    )
--                    state.uploads
--    in
--    ( State
--        { state
--            | update = state.reqading ++ newRequests
--            , requestId = UploadId.update (List.length reading) state.requestId
--        }
--    , readCmds newRequests
--    )
--requests : UploadId -> List Drag.File -> List UploadingFile
--requests requestId =
--    List.indexedMap (\i file -> UploadingFile (UploadId.update i requestId) file)
--
--
--base64EncodeFiles : List Drag.File -> State file -> ( State file, Cmd msg )
--base64EncodeFiles files (State state) =
--    let
--        newRequests =
--            File.requests (UploadId.update 1 state.requestId) files
--
--        uploads =
--            files
--                |> List.indexedMap (,)
--                |> List.foldl
--                    (\acc ( index, file ) ->
--                        UploadId.insert (UploadId.update index state.requestId) file acc
--                    )
--                    state.uploads
--    in
--    ( State
--        { state
--            | update = state.reqading ++ newRequests
--            , requestId = UploadId.update (List.length reading) state.requestId
--        }
--    , readCmds newRequests
--    )


base64EncodeFiles : List Drag.File -> State file -> ( State file, Cmd msg )
base64EncodeFiles files (State state) =
    let
        ( uploads, insertedIds ) =
            files
                |> List.indexedMap
                    (\i file ->
                        ( UploadId.update i state.requestId
                        , UploadingFile file ReadingBase64
                        )
                    )
                |> List.foldl
                    (\( id, file ) ( collection, insertedId ) ->
                        ( UploadId.insert id file collection
                        , id :: insertedIds
                        )
                    )
                    ( state.uploads, [] )
    in
    ( State
        { state
            | uploads = uploads
            , requestId = UploadId.update (List.length files) state.requestId
        }
    , readCmds insertedIds uploads
    )


readCmds : List UploadId -> UploadId.Collection (UploadingFile file) -> Cmd msg
readCmds uploadIds collection =
    uploadIds
        |> List.filterMap
            (\id ->
                collection
                    |> UploadId.get id
                    |> Maybe.map
                        (\(UploadingFile { data } _) ->
                            readFileContent ( UploadId.encoder id, data )
                        )
            )
        |> Cmd.batch


fileReadSuccess : UploadId -> UploadingFile file -> State file -> State file
fileReadSuccess uploadId file (State state) =
    State <|
        { state | uploads = UploadId.insert uploadId file state.uploads }



--
--
--fileUploadSuccess : UploadId -> State file -> ( State file, Maybe file )
--fileUploadSuccess requestId (State state) =
--    let
--        ( uploading, maybeBackendFile ) =
--            File.popUploadingRequest requestId state.uploading
--    in
--    ( State { state | uploading = uploading }
--    , maybeBackendFile
--    )
--
--


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
                    ( UploadId.insert uploadId uploadingFile state.uploads
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
                        upload ( UploadId.encoder id, SignedUrl.toString signedUrl, base64 )

                    _ ->
                        Cmd.none
            )
        |> Cmd.batch



--
--
--updateS3UploadProgress : UploadId -> Float -> State file -> State file
--updateS3UploadProgress id progress (State state) =
--    State <|
--        { state | uploading = File.updateUploadProgress id progress state.uploading }


cancelTrigger : Config msg file -> UploadState file -> msg
cancelTrigger (Config { cancelUploadMsg }) file =
    cancelUploadMsg file



--
--cancelUpload : UploadState file -> State file -> ( State file, Cmd msg )
--cancelUpload file (State state) =
--    case file of
--        ReadingBase64 request ->
--            ( State { state | reading = File.removeReadRequest request state.reading }
--            , Cmd.none
--            )
--
--        GettingSignedS3Url response ->
--            ( State { state | signing = File.removeSigningRequest response state.signing }
--            , uploadCancelled (UploadId.encoder (File.idFromResponse response))
--            )
--
--        UploadingToS3 signed ->
--            ( State { state | uploading = File.removeUploadingRequest signed state.uploading }
--            , uploadCancelled (UploadId.encoder (File.idFromSigned signed))
--            )
--
--        _ ->
--            ( State state, Cmd.none )
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
                            |> Pipeline.required "result" Decode.string
                            |> Decode.andThen (UploadingFile rawFile >> (,) requestId >> Decode.succeed)

                    _ ->
                        Decode.fail "Can't find request"
            )
