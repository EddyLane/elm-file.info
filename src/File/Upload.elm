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
        , dropzoneAttrs
        , fileContentRead
        , fileName
        , fileUploadFailure
        , fileUploadSuccess
        , init
        , inputId
        , isFailed
        , isImage
        , maximumFileSize
        , onChangeFiles
        , signedUrlMetadataEncoder
        , updateFileState
        , updateS3UploadProgress
        , uploadCancelled
        , uploadFailed
        , uploadPercentage
        , uploadProgress
        , uploadToUrl
        , uploaded
        , uploads
        , view
        )

{-| Provides an interface to upload files to a remote destination, but needs a whole bunch of wiring to hook it up

The reason this package makes you fill in so many blanks is because it is a trick subject.
There are many ways to do uploads, so we hope that you can fill in the blanks.


# Uploader

When you need to create an uploader you first need to init the state:

    import File.Upload as Upload

    -- You need to keep track of the uploader state in your model

    type alias Model =
        { upload : Upload.State }


    -- The uploader needs to be initialized

    initialState : Upload.State
    initialState =
        { upload = Upload.init }


    -- You need to then configure the uploader, starting with a NoOp message; a custom type which takes no arguments.

    uploadConfig : Upload.Config Msg
    uploadConfig =
        Upload.config NoOp

    -- You can then add the uploader to your view

    Upload.view state config

-}

--import File.Data.SignedUrl as SignedUrl exposing (SignedUrl)

import Drag
import File.Data.Base64Encoded as Base64Encoded exposing (Base64Encoded)
import File.Data.UploadId as UploadId exposing (UploadId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onWithOptions)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


--- PORTS -----


{-| A port used to trigger the onClick event on an actual file input, specified by the String param
-}
port browseClick : String -> Cmd msg


{-| A port used to update the progress of the upload from JS-land; Encode.Value is the UploadId and Float is the percent
-}
port uploadProgress : (( Encode.Value, Float ) -> msg) -> Sub msg


{-| A port used to cancel the upload in JS-land. Encode.Value is the UploadId
-}
port uploadCancelled : Encode.Value -> Cmd msg


{-| A port used to tell JS-land to read the Base64 file content of a file.
The Encode.Value is the UploadId
The Decode.Value is the raw JS file event. For more details see the Drag.File documentation.
-}
port readFileContent : ( Encode.Value, Decode.Value ) -> Cmd msg


{-| A port used to update the internal file with the Base64 encoded file
-}
port fileContentRead : (Encode.Value -> msg) -> Sub msg


{-| A port used to tell JS-land to actually upload a file to S3. Sends the UploadId, SignedUrl and Base64Encoded data
The encode values are:

  - UploadId
  - SignedUrl
  - Base64Encoded

-}
port upload : ( Encode.Value, Encode.Value, Encode.Value, Encode.Value ) -> Cmd msg


{-| A port used to tell the internal state that an upload has failed, and to update accordingly}
-}
port uploadFailed : (Encode.Value -> msg) -> Sub msg


{-| A port used to tell the internal state that the file has been successfully uploaded
-}
port uploaded : (( Encode.Value, Encode.Value ) -> msg) -> Sub msg



---- STATE ----


type UploadingFile
    = UploadingFile Drag.File UploadStatus


type UploadStatus
    = ReadingBase64
    | Uploading Base64Encoded Float
    | Failed


type State
    = State StateRec


{-| State used to represent this uploader

    - `dropActive` Is the DropZone currently 'active'; files are hovering over ready to be dropped
    - `uploads` Is the current collection of uploading files

-}
type alias StateRec =
    { dropActive : Bool
    , uploads : UploadId.Collection UploadingFile
    }


uploads : State -> UploadId.Collection UploadingFile
uploads (State { uploads }) =
    uploads



---- CONFIG ----


type Config msg
    = Config (ConfigRec msg)


{-| Configuration information for describing the behaviour of the Uploader

    - `onChangeFilesMsg` Msg called with the ID of the multiple file input and a list of files when input changes
    - `browseClickMsg` Msg called with the ID of the multiple file input to simulate a click on the input
    - `dragOverMsg` Msg called when dragging files over the dropzone
    - `dragLeaveMsg` Msg called when dragging files out of the dropzone
    - `dropMsg` Msg called when dropping a collection of files on the dropzone
    - `maximumFileSize` The maximum size of files to upload, **NOT CURRENTLY USED**
    - `inputId` The ID of the actual form input that is used.

-}
type alias ConfigRec msg =
    { onChangeFilesMsg : String -> List Drag.File -> msg
    , browseClickMsg : String -> msg
    , dragOverMsg : Drag.Event -> msg
    , dragLeaveMsg : Drag.Event -> msg
    , dropMsg : Drag.Event -> msg
    , maximumFileSize : Int
    , inputId : String
    , dropZoneAttrs : List (Attribute msg)
    }


{-| Init the uploader
-}
init : State
init =
    State <|
        { dropActive = False
        , uploads = UploadId.init
        }


{-| Init the configuration of this uploader with a no-op msg
-}
config : msg -> Config msg
config noOpMsg =
    Config <|
        { onChangeFilesMsg = always (always noOpMsg)
        , browseClickMsg = always noOpMsg
        , dragOverMsg = always noOpMsg
        , dragLeaveMsg = always noOpMsg
        , dropMsg = always noOpMsg
        , maximumFileSize = 5000
        , inputId = "elm-file-upload-input"
        , dropZoneAttrs = []
        }


{-| Set attributes for the dropzone
-}
dropzoneAttrs : List (Attribute msg) -> Config msg -> Config msg
dropzoneAttrs dropZoneAttrs (Config configRec) =
    Config <|
        { configRec | dropZoneAttrs = dropZoneAttrs }


{-| Set what happens when files are added to the upload queue
-}
onChangeFiles : (String -> List Drag.File -> msg) -> Config msg -> Config msg
onChangeFiles msg (Config configRec) =
    Config <|
        { configRec | onChangeFilesMsg = msg }


{-| Set the id of the input element used to upload files. If multiple uploaders this should be unique
-}
inputId : String -> Config msg -> Config msg
inputId inputId (Config configRec) =
    Config <|
        { configRec | inputId = inputId }


{-| Set what happens when manually triggering the uploader (i.e. from an anchor tag)
-}
browseFiles : (String -> msg) -> Config msg -> Config msg
browseFiles msg (Config configRec) =
    Config <|
        { configRec | browseClickMsg = msg }


{-| Set what happens when dragging over, dragging out and dropping on the drop zone
-}
drag : (Drag.Event -> msg) -> (Drag.Event -> msg) -> (Drag.Event -> msg) -> Config msg -> Config msg
drag over leave drop (Config configRec) =
    Config <|
        { configRec
            | dragOverMsg = over
            , dragLeaveMsg = leave
            , dropMsg = drop
        }


{-| Set the maximum size of the uploaded files **NOT CURRENTLY USED**
-}
maximumFileSize : Int -> Config msg -> Config msg
maximumFileSize size (Config configRec) =
    Config <|
        { configRec | maximumFileSize = size }



---- FILE ----


{-| Get the filename for an uploading file
-}
fileName : UploadingFile -> String
fileName (UploadingFile { name } _) =
    name


{-| Is the upload failed?
-}
isFailed : UploadingFile -> Bool
isFailed (UploadingFile _ uploadState) =
    case uploadState of
        Failed ->
            True

        _ ->
            False


{-| Is the uploading file an image?
-}
isImage : UploadingFile -> Bool
isImage (UploadingFile { typeMIME } _) =
    String.startsWith "image" typeMIME


{-| Get the percentage that the uploading file has uploaded
-}
uploadPercentage : UploadingFile -> Float
uploadPercentage file =
    case file of
        UploadingFile _ (Uploading _ percentage) ->
            percentage

        _ ->
            0.0


{-| Get the base64 data for an uploading file, if ready.
-}
base64EncodedData : UploadingFile -> Maybe Base64Encoded
base64EncodedData (UploadingFile file status) =
    case status of
        ReadingBase64 ->
            Nothing

        Uploading base64Encoded _ ->
            Just base64Encoded

        Failed ->
            Nothing



---- UPDATE ----


{-| Update the active state of the drop zone
-}
dropActive : Bool -> State -> State
dropActive isActive (State state) =
    State { state | dropActive = isActive }


{-| Start a list of files uploading. Returns tuple with state of the uploader with the new files and Cmds for ports
-}
base64EncodeFiles : List Drag.File -> State -> ( State, Cmd msg )
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


readCmds : List UploadId -> UploadId.Collection UploadingFile -> Cmd msg
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


updateFileState : UploadId -> UploadingFile -> State -> State
updateFileState uploadId file (State state) =
    State { state | uploads = UploadId.update uploadId (always <| Just file) state.uploads }


{-| Updates a particular uploading file when it the base64 data has been successfully read from JS-land
-}
uploadToUrl : Encode.Value -> Encode.Value -> UploadId -> State -> Cmd msg
uploadToUrl uploadUrl additionalData uploadId (State state) =
    case UploadId.get uploadId state.uploads of
        Just (UploadingFile rawFile (Uploading base64 _)) ->
            upload
                ( UploadId.encoder uploadId
                , uploadUrl
                , Base64Encoded.encoder base64
                , additionalData
                )

        _ ->
            Cmd.none



--{-| Removes a particular uploading file when the data has been successfully uploaded to the end destination
--
--Returns both the new state of the uploader and the file that has been uploaded
--
---}


fileUploadSuccess : UploadId -> State -> State
fileUploadSuccess uploadId (State state) =
    State { state | uploads = UploadId.remove uploadId state.uploads }


fileUploadFailure : UploadId -> State -> State
fileUploadFailure requestId (State state) =
    State <|
        { state
            | uploads =
                UploadId.update requestId
                    (Maybe.map
                        (\(UploadingFile rawFile _) ->
                            UploadingFile rawFile Failed
                        )
                    )
                    state.uploads
        }


{-| Updates the progress of an upload to S3 from JS-land with a new percentage
-}
updateS3UploadProgress : UploadId -> Float -> State -> State
updateS3UploadProgress id progress (State state) =
    State <|
        { state
            | uploads =
                UploadId.update id
                    (Maybe.map
                        (\upload ->
                            case upload of
                                UploadingFile rawFile (Uploading base64 _) ->
                                    UploadingFile rawFile (Uploading base64 progress)

                                _ ->
                                    upload
                        )
                    )
                    state.uploads
        }


{-| Cancel an upload specified by the UploadId
Returns a tuple with:

  - The new internal state of the uploader, with the file removed
  - Cmds to cancel both the upload and any artifacts created during the upload process

-}
cancelUpload : UploadId -> State -> ( State, Cmd msg )
cancelUpload uploadId (State state) =
    ( State { state | uploads = UploadId.remove uploadId state.uploads }
    , uploadCancelled (UploadId.encoder uploadId)
    )



---- VIEW ----


view : State -> Config msg -> Html msg
view (State state) (Config config) =
    div config.dropZoneAttrs
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



---- ENCODER ----


signedUrlMetadataEncoder : UploadingFile -> Encode.Value
signedUrlMetadataEncoder (UploadingFile { typeMIME, name, size } _) =
    Encode.object
        [ ( "contentType", Encode.string typeMIME )
        , ( "fileName", Encode.string name )
        , ( "size", Encode.int size )
        ]


base64PortDecoder : State -> Decode.Decoder ( UploadId, UploadingFile )
base64PortDecoder (State { uploads }) =
    Decode.field "id" UploadId.decoder
        |> Decode.andThen
            (\requestId ->
                case UploadId.get requestId uploads of
                    Just (UploadingFile rawFile _) ->
                        Pipeline.decode (\base64 -> Uploading base64 0.0)
                            |> Pipeline.required "result" Base64Encoded.decoder
                            |> Decode.andThen (UploadingFile rawFile >> (,) requestId >> Decode.succeed)

                    _ ->
                        Decode.fail "Can't find request"
            )
