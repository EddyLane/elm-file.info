port module File.Upload
    exposing
        ( Config
        , State
        , UploadingFile
        , cancel
        , config
        , configBase64EncodedMsg
        , configMaximumFileSize
        , configUploadProgressMsg
        , configUploadedMsg
        , encode
        , failure
        , fileData
        , fileFilename
        , fileIsFailed
        , fileIsImage
        , fileProgress
        , init
        , progress
        , subscriptions
        , success
        , update
        , upload
        , uploadCancelled
        , uploadProgress
        , uploaded
        , uploads
        )

{-| Provides an interface to upload files to a remote destination, but needs a whole bunch of wiring to hook it up

The reason this package makes you fill in so many blanks is because it is a tricky subject.
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


port fileContentReadFailed : (Encode.Value -> msg) -> Sub msg


{-| A port used to update the internal file with the Base64 encoded file
-}
port fileContentRead : (Encode.Value -> msg) -> Sub msg


{-| A port used to tell JS-land to actually upload a file to S3. Sends the UploadId, SignedUrl and Base64Encoded data
The encode values are:fileContentRead

  - UploadId
  - SignedUrl
  - Base64Encoded

-}
port uploadPort : ( Encode.Value, Encode.Value, Encode.Value, Encode.Value ) -> Cmd msg


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

    - `dragOverMsg` Msg called when dragging files over the dropzone
    - `dragLeaveMsg` Msg called when dragging files out of the dropzone
    - `dropMsg` Msg called when dropping a collection of files on the dropzone
    - `maximumFileSize` The maximum size of files to upload

-}
type alias ConfigRec msg =
    { browseClickMsg : String -> msg
    , dragMsg : Bool -> msg
    , maximumFileSize : Int
    , uploadProgressMsg : UploadId -> Float -> msg
    , uploadedMsg : Result UploadId ( UploadId, Encode.Value ) -> msg
    , base64EncodedMsg : Result UploadId ( UploadId, UploadingFile ) -> msg
    , noOpMsg : msg
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
        { browseClickMsg = always noOpMsg
        , dragMsg = always noOpMsg
        , maximumFileSize = 5000
        , uploadProgressMsg = always (always noOpMsg)
        , uploadedMsg = always noOpMsg
        , noOpMsg = noOpMsg
        , base64EncodedMsg = always noOpMsg
        }


configUploadProgressMsg : (UploadId -> Float -> msg) -> Config msg -> Config msg
configUploadProgressMsg uploadProgressMsg (Config configRec) =
    Config <|
        { configRec | uploadProgressMsg = uploadProgressMsg }


configUploadedMsg : (Result UploadId ( UploadId, Encode.Value ) -> msg) -> Config msg -> Config msg
configUploadedMsg msg (Config configRec) =
    Config <|
        { configRec | uploadedMsg = msg }


configBase64EncodedMsg : (Result UploadId ( UploadId, UploadingFile ) -> msg) -> Config msg -> Config msg
configBase64EncodedMsg msg (Config configRec) =
    Config <|
        { configRec | base64EncodedMsg = msg }


{-| Set the maximum size of the uploaded files
-}
configMaximumFileSize : Int -> Config msg -> Config msg
configMaximumFileSize size (Config configRec) =
    Config <|
        { configRec | maximumFileSize = size }



---- FILE ----


{-| Get the filename for an uploading file
-}
fileFilename : UploadingFile -> String
fileFilename (UploadingFile { name } _) =
    name


{-| Is the upload failed?
-}
fileIsFailed : UploadingFile -> Bool
fileIsFailed (UploadingFile _ uploadState) =
    case uploadState of
        Failed ->
            True

        _ ->
            False


{-| Is the uploading file an image?
-}
fileIsImage : UploadingFile -> Bool
fileIsImage (UploadingFile { typeMIME } _) =
    String.startsWith "image" typeMIME


{-| Get the percentage that the uploading file has uploaded
-}
fileProgress : UploadingFile -> Float
fileProgress file =
    case file of
        UploadingFile _ (Uploading _ percentage) ->
            percentage

        _ ->
            0.0


{-| Get the base64 data for an uploading file, if ready.
-}
fileData : UploadingFile -> Maybe Base64Encoded
fileData (UploadingFile file status) =
    case status of
        ReadingBase64 ->
            Nothing

        Uploading base64Encoded _ ->
            Just base64Encoded

        Failed ->
            Nothing



---- UPDATE ----


{-| Start a list of files uploading. Returns tuple with state of the uploader with the new files and Cmds for ports
-}
encode : Config msg -> List Drag.File -> State -> ( State, Cmd msg )
encode (Config config) files (State state) =
    let
        ( updatedUploadCollection, insertedIds ) =
            files
                |> List.map
                    (\file ->
                        UploadingFile file
                            (if file.size > config.maximumFileSize then
                                Failed
                             else
                                ReadingBase64
                            )
                    )
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
    , stateReadCmds insertedIds updatedUploadCollection
    )


stateReadCmds : List UploadId -> UploadId.Collection UploadingFile -> Cmd msg
stateReadCmds uploadIds collection =
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


update : UploadId -> UploadingFile -> State -> State
update uploadId file (State state) =
    State { state | uploads = UploadId.update uploadId (always <| Just file) state.uploads }


{-| Updates a particular uploading file when it the base64 data has been successfully read from JS-land
-}
upload : Encode.Value -> Encode.Value -> UploadId -> State -> Cmd msg
upload uploadUrl additionalData uploadId (State state) =
    case UploadId.get uploadId state.uploads of
        Just (UploadingFile rawFile (Uploading base64 _)) ->
            uploadPort
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


success : UploadId -> State -> State
success uploadId (State state) =
    State { state | uploads = UploadId.remove uploadId state.uploads }


failure : UploadId -> State -> State
failure requestId (State state) =
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
progress : UploadId -> Float -> State -> State
progress id progress (State state) =
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
cancel : UploadId -> State -> ( State, Cmd msg )
cancel uploadId (State state) =
    ( State { state | uploads = UploadId.remove uploadId state.uploads }
    , uploadCancelled (UploadId.encoder uploadId)
    )



---- ENCODER ----


metadataEncoder : UploadingFile -> Encode.Value
metadataEncoder (UploadingFile { typeMIME, name, size } _) =
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



---- SUBSCRIPTIONS ----


subscriptions : State -> Config msg -> Sub msg
subscriptions state config =
    Sub.batch
        [ fileUploadedSub config
        , fileFailureSub config
        , fileUploadProgressSub config
        , base64EncodeFileSub state config
        , readFileContentFailedSub config
        ]


base64EncodeFileSub : State -> Config msg -> Sub msg
base64EncodeFileSub state (Config { base64EncodedMsg, noOpMsg }) =
    fileContentRead
        (\encodedValue ->
            case Decode.decodeValue (base64PortDecoder state) encodedValue of
                Ok upload ->
                    base64EncodedMsg (Ok upload)

                Err _ ->
                    noOpMsg
        )


fileUploadedSub : Config msg -> Sub msg
fileUploadedSub (Config { noOpMsg, uploadedMsg }) =
    uploaded
        (\( encodedId, encodedAttachment ) ->
            case Decode.decodeValue UploadId.decoder encodedId of
                Ok uploadId ->
                    uploadedMsg (Ok ( uploadId, encodedAttachment ))

                Err _ ->
                    noOpMsg
        )


readFileContentFailedSub : Config msg -> Sub msg
readFileContentFailedSub (Config { noOpMsg, base64EncodedMsg }) =
    fileContentReadFailed
        (Decode.decodeValue UploadId.decoder
            >> Result.toMaybe
            >> Maybe.map (Err >> base64EncodedMsg)
            >> Maybe.withDefault noOpMsg
        )


fileFailureSub : Config msg -> Sub msg
fileFailureSub (Config { noOpMsg, uploadedMsg }) =
    uploadFailed
        (Decode.decodeValue UploadId.decoder
            >> Result.toMaybe
            >> Maybe.map (Err >> uploadedMsg)
            >> Maybe.withDefault noOpMsg
        )


fileUploadProgressSub : Config msg -> Sub msg
fileUploadProgressSub (Config { noOpMsg, uploadProgressMsg }) =
    uploadProgress
        (\( id, progress ) ->
            case Decode.decodeValue UploadId.decoder id of
                Ok uploadId ->
                    uploadProgressMsg uploadId progress

                Err _ ->
                    noOpMsg
        )
