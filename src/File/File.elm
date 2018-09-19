port module File.File
    exposing
        ( FileLifecycle
        , FileReadPortRequest
        , FileReadPortResponse
        , FileSigned
        , file
        , fileContentRead
        , filePortDecoder
        , lifeCycleReading
        , lifeCycleSigning
        , lifeCycleUploading
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
import Date.Extra
import Drag
import File.SignedUrl as SignedUrl exposing (SignedUrl)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


---- PORTS ----


port fileContentRead : (Encode.Value -> msg) -> Sub msg


port readFileContent : ( Int, String, Decode.Value ) -> Cmd msg


port upload : ( Int, String, String ) -> Cmd msg



---- DATA ----


type FileReadPortRequest
    = FileReadPortRequest Int String Drag.File


type FileReadPortResponse
    = FileReadPortResponse FileReadPortRequest String


type FileSigned
    = FileSigned FileReadPortResponse SignedUrl


type FileLifecycle
    = ReadingBase64 FileReadPortRequest
    | GettingSignedS3Url FileReadPortResponse
    | UploadingToS3 FileSigned


lifeCycleReading : FileReadPortRequest -> FileLifecycle
lifeCycleReading =
    ReadingBase64


lifeCycleSigning : FileReadPortResponse -> FileLifecycle
lifeCycleSigning =
    GettingSignedS3Url


lifeCycleUploading : FileSigned -> FileLifecycle
lifeCycleUploading =
    UploadingToS3


requests : Int -> String -> List Drag.File -> List FileReadPortRequest
requests requestId inputId =
    List.indexedMap (\i file -> request (i + requestId) inputId file)


readCmds : List FileReadPortRequest -> Cmd msg
readCmds requests =
    requests
        |> List.map
            (\(FileReadPortRequest id inputId request) ->
                readFileContent ( id, inputId, request.data )
            )
        |> Cmd.batch


uploadCmds : List FileSigned -> Cmd msg
uploadCmds signed =
    signed
        |> List.map
            (\(FileSigned (FileReadPortResponse (FileReadPortRequest id _ _) base64File) signedUrl) ->
                upload ( id, SignedUrl.toString signedUrl, base64File )
            )
        |> Cmd.batch


uploadProgress : FileLifecycle -> Float
uploadProgress file =
    case file of
        ReadingBase64 _ ->
            0.0

        GettingSignedS3Url _ ->
            10.0

        UploadingToS3 _ ->
            40.0


removeReadRequest : FileReadPortResponse -> List FileReadPortRequest -> List FileReadPortRequest
removeReadRequest (FileReadPortResponse (FileReadPortRequest requestId _ _) _) =
    List.filter (\(FileReadPortRequest id _ _) -> id /= requestId)


removeSigningRequest : FileSigned -> List FileReadPortResponse -> List FileReadPortResponse
removeSigningRequest (FileSigned (FileReadPortResponse (FileReadPortRequest requestId _ _) _) _) =
    List.filter (\(FileReadPortResponse (FileReadPortRequest id _ _) _) -> id /= requestId)


request : Int -> String -> Drag.File -> FileReadPortRequest
request =
    FileReadPortRequest


signed : FileReadPortResponse -> SignedUrl -> FileSigned
signed =
    FileSigned


thumbnailSrc : FileLifecycle -> String
thumbnailSrc file =
    case ( isImage file, file ) of
        ( True, GettingSignedS3Url response ) ->
            base64Encoded response

        ( True, UploadingToS3 (FileSigned response _) ) ->
            base64Encoded response

        _ ->
            ""


isImage : FileLifecycle -> Bool
isImage file =
    case file of
        ReadingBase64 (FileReadPortRequest _ _ _) ->
            False

        GettingSignedS3Url (FileReadPortResponse (FileReadPortRequest _ _ { typeMIME }) _) ->
            String.startsWith "image" typeMIME

        UploadingToS3 (FileSigned (FileReadPortResponse (FileReadPortRequest _ _ { typeMIME }) _) _) ->
            String.startsWith "image" typeMIME


file : FileLifecycle -> Drag.File
file file =
    case file of
        ReadingBase64 request ->
            fileFromRequest request

        GettingSignedS3Url response ->
            fileFromResponse response

        UploadingToS3 signed ->
            fileFromSigned signed


fileFromSigned : FileSigned -> Drag.File
fileFromSigned (FileSigned response _) =
    fileFromResponse response


fileFromResponse : FileReadPortResponse -> Drag.File
fileFromResponse (FileReadPortResponse request _) =
    fileFromRequest request


fileFromRequest : FileReadPortRequest -> Drag.File
fileFromRequest (FileReadPortRequest _ _ file) =
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
                            |> List.filter (\(FileReadPortRequest id _ _) -> id == requestId)
                            |> List.head
                in
                case maybeRequest of
                    Just request ->
                        Pipeline.decode (FileReadPortResponse request)
                            |> Pipeline.required "result" Decode.string

                    Nothing ->
                        Decode.fail "Can't find request"
            )
