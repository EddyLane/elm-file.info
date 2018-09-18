port module File.File
    exposing
        ( FileReadPortRequest
        , FileReadPortResponse
        , FileSigned
        , base64Encoded
        , file
        , fileContentRead
        , filePortDecoder
        , isImage
        , readCmds
        , removeReadRequest
        , removeSigningRequest
        , request
        , requests
        , signed
        , uploadCmds
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


isImage : FileReadPortResponse -> Bool
isImage (FileReadPortResponse (FileReadPortRequest _ _ { typeMIME }) _) =
    String.startsWith "image" typeMIME


file : FileReadPortResponse -> Drag.File
file (FileReadPortResponse (FileReadPortRequest _ _ file) _) =
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
