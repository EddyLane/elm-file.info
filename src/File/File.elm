port module File.File
    exposing
        ( FileReadPortRequest
        , FileReadPortResponse
        , FileSigned
        , UploadState
        , fileContentRead
        , filePortDecoder
        , lifeCycleReading
        , lifeCycleSigning
        , lifeCycleUploaded
        , lifeCycleUploading
        , name
        , readCmds
        , removeReadRequest
        , removeSigningRequest
        , request
        , requests
        , signed
        , thumbnailSrc
        , uploadCmds
        , uploadProgress
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


port uploaded : ( Int, Bool ) -> Cmd msg



---- DATA ----


type FileReadPortRequest
    = FileReadPortRequest Int Drag.File


type FileReadPortResponse
    = FileReadPortResponse FileReadPortRequest String


type FileSigned file
    = FileSigned FileReadPortResponse SignedUrl file


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
            (\(FileSigned (FileReadPortResponse (FileReadPortRequest id _) base64File) signedUrl _) ->
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

        UploadingToS3 _ ->
            40.0

        Uploaded _ ->
            100.0


removeReadRequest : FileReadPortResponse -> List FileReadPortRequest -> List FileReadPortRequest
removeReadRequest (FileReadPortResponse (FileReadPortRequest requestId _) _) =
    List.filter (\(FileReadPortRequest id _) -> id /= requestId)


removeSigningRequest : FileSigned file -> List FileReadPortResponse -> List FileReadPortResponse
removeSigningRequest (FileSigned (FileReadPortResponse (FileReadPortRequest requestId _) _) _ _) =
    List.filter (\(FileReadPortResponse (FileReadPortRequest id _) _) -> id /= requestId)


request : Int -> Drag.File -> FileReadPortRequest
request =
    FileReadPortRequest


signed : FileReadPortResponse -> SignedUrl -> file -> FileSigned file
signed =
    FileSigned


thumbnailSrc : UploadState file -> String
thumbnailSrc file =
    case ( isImage file, file ) of
        ( True, GettingSignedS3Url response ) ->
            base64Encoded response

        ( True, UploadingToS3 (FileSigned response _ _) ) ->
            base64Encoded response

        _ ->
            ""


isImage : UploadState file -> Bool
isImage file =
    case file of
        ReadingBase64 (FileReadPortRequest _ _) ->
            False

        GettingSignedS3Url (FileReadPortResponse (FileReadPortRequest _ { typeMIME }) _) ->
            String.startsWith "image" typeMIME

        UploadingToS3 (FileSigned (FileReadPortResponse (FileReadPortRequest _ { typeMIME }) _) _ _) ->
            String.startsWith "image" typeMIME

        Uploaded _ ->
            False


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
fileFromSigned (FileSigned response _ _) =
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



---- ENCODING ----


filePortDecoder : List FileReadPortRequest -> Decode.Decoder FileReadPortResponse
filePortDecoder requests =
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
