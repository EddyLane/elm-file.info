port module File.File
    exposing
        ( FileReadPortRequest
        , FileReadPortResponse
        , FileSigned
        , UploadState
        , base64PortDecoder
        , fileContentRead
        , lifeCycleReading
        , lifeCycleSigning
        , lifeCycleUploaded
        , lifeCycleUploading
        , name
        , popUploadingRequest
        , readCmds
        , removeReadRequest
        , removeSigningRequest
        , request
        , requests
        , signed
        , signedUrlMetadataEncoder
        , thumbnailSrc
        , updateUploadProgress
        , uploadCmds
        , uploadProgress
        , uploaded
        )

import Date exposing (Date)
import Drag
import File.SignedUrl as SignedUrl exposing (SignedUrl)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


---- PORTS ----


port fileContentRead : (Encode.Value -> msg) -> Sub msg


port readFileContent : ( Int, Decode.Value ) -> Cmd msg


port upload : ( Int, String, String ) -> Cmd msg


port uploaded : (Int -> msg) -> Sub msg



---- DATA ----


type FileReadPortRequest
    = FileReadPortRequest Int Drag.File


type FileReadPortResponse
    = FileReadPortResponse FileReadPortRequest String


type FileSigned file
    = FileSigned FileReadPortResponse SignedUrl Float file


type UploadState file
    = ReadingBase64 FileReadPortRequest
    | GettingSignedS3Url FileReadPortResponse
    | UploadingToS3 (FileSigned file)
    | Uploaded file


lifeCycleReading : FileReadPortRequest -> UploadState file
lifeCycleReading =
    ReadingBase64


lifeCycleSigning : FileReadPortResponse -> UploadState file
lifeCycleSigning =
    GettingSignedS3Url


lifeCycleUploading : FileSigned file -> UploadState file
lifeCycleUploading =
    UploadingToS3


lifeCycleUploaded : file -> UploadState file
lifeCycleUploaded =
    Uploaded


requests : Int -> List Drag.File -> List FileReadPortRequest
requests requestId =
    List.indexedMap (\i file -> request (i + requestId) file)


readCmds : List FileReadPortRequest -> Cmd msg
readCmds requests =
    requests
        |> List.map
            (\(FileReadPortRequest id request) ->
                readFileContent ( id, request.data )
            )
        |> Cmd.batch


uploadCmds : List (FileSigned file) -> Cmd msg
uploadCmds signed =
    signed
        |> List.map
            (\(FileSigned (FileReadPortResponse (FileReadPortRequest id _) base64File) signedUrl _ _) ->
                upload ( id, SignedUrl.toString signedUrl, base64File )
            )
        |> Cmd.batch


uploadProgress : UploadState file -> Float
uploadProgress file =
    case file of
        ReadingBase64 _ ->
            0.0

        GettingSignedS3Url _ ->
            10.0

        UploadingToS3 (FileSigned _ _ progress _) ->
            progress

        Uploaded _ ->
            100.0


removeReadRequest : FileReadPortResponse -> List FileReadPortRequest -> List FileReadPortRequest
removeReadRequest (FileReadPortResponse (FileReadPortRequest requestId _) _) =
    List.filter (\(FileReadPortRequest id _) -> id /= requestId)


removeSigningRequest : FileSigned file -> List FileReadPortResponse -> List FileReadPortResponse
removeSigningRequest (FileSigned (FileReadPortResponse (FileReadPortRequest requestId _) _) _ _ _) =
    List.filter (\(FileReadPortResponse (FileReadPortRequest id _) _) -> id /= requestId)


popUploadingRequest : Int -> List (FileSigned file) -> ( List (FileSigned file), Maybe file )
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


request : Int -> Drag.File -> FileReadPortRequest
request =
    FileReadPortRequest


signed : FileReadPortResponse -> SignedUrl -> Float -> file -> FileSigned file
signed =
    FileSigned


thumbnailSrc : (file -> String) -> (file -> String) -> UploadState file -> String
thumbnailSrc thumbnailSrcFn contentTypeFn file =
    case ( isImage contentTypeFn file, file ) of
        ( True, GettingSignedS3Url response ) ->
            base64Encoded response

        ( True, UploadingToS3 (FileSigned response _ _ _) ) ->
            base64Encoded response

        ( True, Uploaded file ) ->
            thumbnailSrcFn file

        _ ->
            ""


isImage : (file -> String) -> UploadState file -> Bool
isImage contentTypeFn file =
    case file of
        ReadingBase64 (FileReadPortRequest _ _) ->
            False

        GettingSignedS3Url (FileReadPortResponse (FileReadPortRequest _ { typeMIME }) _) ->
            String.startsWith "image" typeMIME

        UploadingToS3 (FileSigned (FileReadPortResponse (FileReadPortRequest _ { typeMIME }) _) _ _ _) ->
            String.startsWith "image" typeMIME

        Uploaded backendFile ->
            String.startsWith "image" (contentTypeFn backendFile)


name : (file -> String) -> UploadState file -> String
name uploadedFn file =
    case file of
        ReadingBase64 request ->
            .name <| fileFromRequest request

        GettingSignedS3Url response ->
            .name <| fileFromResponse response

        UploadingToS3 signed ->
            .name <| fileFromSigned signed

        Uploaded uploaded ->
            uploadedFn uploaded


fileFromSigned : FileSigned file -> Drag.File
fileFromSigned (FileSigned response _ _ _) =
    fileFromResponse response


fileFromResponse : FileReadPortResponse -> Drag.File
fileFromResponse (FileReadPortResponse request _) =
    fileFromRequest request


fileFromRequest : FileReadPortRequest -> Drag.File
fileFromRequest (FileReadPortRequest _ file) =
    file


base64Encoded : FileReadPortResponse -> String
base64Encoded (FileReadPortResponse _ base64Encoded) =
    base64Encoded


updateUploadProgress : Int -> Float -> List (FileSigned file) -> List (FileSigned file)
updateUploadProgress requestId newProgress =
    List.map
        (\((FileSigned (FileReadPortResponse (FileReadPortRequest id rawFile) base64File) signedUrl oldProgress backendFile) as file) ->
            if id == requestId then
                FileSigned (FileReadPortResponse (FileReadPortRequest id rawFile) base64File) signedUrl newProgress backendFile
            else
                file
        )



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
    Decode.field "id" Decode.int
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
