port module File.File
    exposing
        ( FileReadPortRequest
        , FileReadPortResponse
        , FileSigned
        , base64EncodedSigning
        , base64EncodedUploading
        , base64PortDecoder
        , fileContentRead
        , fileFromRequest
        , fileFromResponse
        , fileFromSigned
        , fileReadSuccess
        , fileSigningSuccess
        , idFromRequest
        , idFromResponse
        , idFromSigned
        , isImageReading
        , isImageSigning
        , isImageUploading
        , popUploadingRequest
        , progress
        , readCmds
        , removeReadRequest
        , removeSigningRequest
        , removeUploadingRequest
        , request
        , requests
        , signed
        , signedUrlMetadataEncoder
        , updateUploadProgress
        , uploadCmds
        , uploaded
        )

import Drag
import File.SignedUrl as SignedUrl exposing (SignedUrl)
import File.UploadId as UploadId exposing (UploadId)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


---- PORTS ----


port fileContentRead : (Encode.Value -> msg) -> Sub msg


port readFileContent : ( Encode.Value, Decode.Value ) -> Cmd msg


port upload : ( Encode.Value, String, String ) -> Cmd msg


port uploaded : (Encode.Value -> msg) -> Sub msg



---- DATA ----


type FileReadPortRequest
    = FileReadPortRequest UploadId Drag.File


type FileReadPortResponse
    = FileReadPortResponse FileReadPortRequest String


type FileSigned file
    = FileSigned FileReadPortResponse SignedUrl Float file


requests : UploadId -> List Drag.File -> List FileReadPortRequest
requests requestId =
    List.indexedMap (\i file -> FileReadPortRequest (UploadId.update i requestId) file)


readCmds : List FileReadPortRequest -> Cmd msg
readCmds requests =
    requests
        |> List.map
            (\(FileReadPortRequest id request) ->
                readFileContent ( UploadId.encoder id, request.data )
            )
        |> Cmd.batch


uploadCmds : List (FileSigned file) -> Cmd msg
uploadCmds signed =
    signed
        |> List.map
            (\(FileSigned (FileReadPortResponse (FileReadPortRequest id _) base64File) signedUrl _ _) ->
                upload ( UploadId.encoder id, SignedUrl.toString signedUrl, base64File )
            )
        |> Cmd.batch


progress : FileSigned file -> Float
progress (FileSigned _ _ progress _) =
    progress


fileReadSuccess : FileReadPortResponse -> List FileReadPortRequest -> List FileReadPortRequest
fileReadSuccess (FileReadPortResponse request _) =
    removeReadRequest request


fileSigningSuccess : FileSigned file -> List FileReadPortResponse -> List FileReadPortResponse
fileSigningSuccess (FileSigned response _ _ _) =
    removeSigningRequest response


removeReadRequest : FileReadPortRequest -> List FileReadPortRequest -> List FileReadPortRequest
removeReadRequest (FileReadPortRequest requestId _) =
    List.filter (\(FileReadPortRequest id _) -> id /= requestId)


removeSigningRequest : FileReadPortResponse -> List FileReadPortResponse -> List FileReadPortResponse
removeSigningRequest (FileReadPortResponse (FileReadPortRequest requestId _) _) =
    List.filter (\(FileReadPortResponse (FileReadPortRequest id _) _) -> id /= requestId)


removeUploadingRequest : FileSigned file -> List (FileSigned file) -> List (FileSigned file)
removeUploadingRequest (FileSigned (FileReadPortResponse (FileReadPortRequest requestId _) _) _ _ _) =
    popUploadingRequest requestId >> Tuple.first


popUploadingRequest : UploadId -> List (FileSigned file) -> ( List (FileSigned file), Maybe file )
popUploadingRequest requestId =
    List.foldl
        (\current ( unchanged, maybeFound ) ->
            let
                (FileSigned (FileReadPortResponse (FileReadPortRequest id _) _) _ _ backendFile) =
                    current
            in
            if requestId == id then
                ( unchanged, Just backendFile )
            else
                ( current :: unchanged, maybeFound )
        )
        ( [], Nothing )


request : UploadId -> Drag.File -> FileReadPortRequest
request =
    FileReadPortRequest


signed : FileReadPortResponse -> SignedUrl -> Float -> file -> FileSigned file
signed =
    FileSigned


isImageReading : FileReadPortRequest -> Bool
isImageReading (FileReadPortRequest _ _) =
    False


isImageSigning : FileReadPortResponse -> Bool
isImageSigning (FileReadPortResponse (FileReadPortRequest _ { typeMIME }) _) =
    String.startsWith "image" typeMIME


isImageUploading : FileSigned file -> Bool
isImageUploading (FileSigned (FileReadPortResponse (FileReadPortRequest _ { typeMIME }) _) _ _ _) =
    String.startsWith "image" typeMIME


fileFromSigned : FileSigned file -> Drag.File
fileFromSigned (FileSigned response _ _ _) =
    fileFromResponse response


fileFromResponse : FileReadPortResponse -> Drag.File
fileFromResponse (FileReadPortResponse request _) =
    fileFromRequest request


fileFromRequest : FileReadPortRequest -> Drag.File
fileFromRequest (FileReadPortRequest _ file) =
    file


idFromSigned : FileSigned file -> UploadId
idFromSigned (FileSigned response _ _ _) =
    idFromResponse response


idFromResponse : FileReadPortResponse -> UploadId
idFromResponse (FileReadPortResponse request _) =
    idFromRequest request


idFromRequest : FileReadPortRequest -> UploadId
idFromRequest (FileReadPortRequest id _) =
    id


base64EncodedSigning : FileReadPortResponse -> String
base64EncodedSigning (FileReadPortResponse _ base64Encoded) =
    base64Encoded


base64EncodedUploading : FileSigned file -> String
base64EncodedUploading (FileSigned (FileReadPortResponse _ base64Encoded) _ _ _) =
    base64Encoded


updateUploadProgress : UploadId -> Float -> List (FileSigned file) -> List (FileSigned file)
updateUploadProgress requestId newProgress =
    List.map
        (\((FileSigned (FileReadPortResponse (FileReadPortRequest id rawFile) base64File) signedUrl oldProgress backendFile) as file) ->
            if id == requestId then
                FileSigned (FileReadPortResponse (FileReadPortRequest id rawFile) base64File) signedUrl newProgress backendFile
            else
                file
        )



--cancelUpload :
--    UploadState file
--    ->
--        { a
--            | reading : List FileReadPortRequest
--            , signing : List FileReadPortResponse
--            , uploading : List (FileSigned file)
--            , uploaded : List file
--        }
--    ->
--        { a
--            | reading : List FileReadPortRequest
--            , signing : List FileReadPortResponse
--            , uploading : List (FileSigned file)
--            , uploaded : List file
--        }
--cancelUpload file state =
--    case file of
--        ReadingBase64 (FileReadPortRequest requestId _) ->
--            { state
--                | reading =
--                    List.filter
--                        (\(FileReadPortRequest id _) ->
--                            id /= requestId
--                        )
--                        state.reading
--            }
--
--        GettingSignedS3Url (FileReadPortResponse (FileReadPortRequest requestId _) _) ->
--            { state
--                | signing =
--                    List.filter
--                        (\(FileReadPortResponse (FileReadPortRequest id _) _) ->
--                            id /= requestId
--                        )
--                        state.signing
--            }
--
--        UploadingToS3 (FileSigned (FileReadPortResponse (FileReadPortRequest requestId _) _) _ _ _) ->
--            { state
--                | uploading =
--                    List.filter
--                        (\(FileSigned (FileReadPortResponse (FileReadPortRequest id _) _) _ _ _) ->
--                            id /= requestId
--                        )
--                        state.uploading
--            }
--
--        Uploaded file ->
--            state
--
--
--        UploadingToS3
--
--        Uploaded
---- ENCODING ----
--encodeFileWithoutMetadata : Drag.File -> Encode.Value
--encodeFileWithoutMetadata file =
--    Encode.object
--        [ ("contentType" => File.mime) ]


signedUrlMetadataEncoder : FileReadPortResponse -> Encode.Value
signedUrlMetadataEncoder (FileReadPortResponse (FileReadPortRequest _ { typeMIME, name, size }) _) =
    Encode.object
        [ ( "contentType", Encode.string typeMIME )
        , ( "fileName", Encode.string name )
        , ( "size", Encode.int size )
        ]


base64PortDecoder : List FileReadPortRequest -> Decode.Decoder FileReadPortResponse
base64PortDecoder requests =
    Decode.field "id" UploadId.decoder
        |> Decode.andThen
            (\requestId ->
                let
                    maybeRequest =
                        requests
                            |> List.filter (\(FileReadPortRequest id _) -> id == requestId)
                            |> List.head
                in
                case maybeRequest of
                    Just request ->
                        Pipeline.decode (FileReadPortResponse request)
                            |> Pipeline.required "result" Decode.string

                    Nothing ->
                        Decode.fail "Can't find request"
            )
