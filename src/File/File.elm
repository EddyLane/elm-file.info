port module File.File
    exposing
        ( FilePortRequest
        , FilePortResponse
        , base64Encoded
        , file
        , fileContentRead
        , filePortDecoder
        , isImage
        , readCmds
        , removeRequest
        , request
        , requests
        )

import Date exposing (Date)
import Date.Extra
import Drag
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


---- PORTS ----


port fileContentRead : (Encode.Value -> msg) -> Sub msg


port readFileContent : ( Int, String, Decode.Value ) -> Cmd msg



---- DATA ----


type FilePortRequest
    = FilePortRequest Int String Drag.File


type FilePortResponse
    = FilePortResponse FilePortRequest String


requests : Int -> String -> List Drag.File -> List FilePortRequest
requests requestId inputId =
    List.indexedMap (\i file -> request (i + requestId) inputId file)


readCmds : List FilePortRequest -> Cmd msg
readCmds requests =
    requests
        |> List.map (\(FilePortRequest id inputId request) -> readFileContent ( id, inputId, request.data ))
        |> Cmd.batch


removeRequest : FilePortResponse -> List FilePortRequest -> List FilePortRequest
removeRequest (FilePortResponse (FilePortRequest requestId _ _) _) =
    List.filter (\(FilePortRequest id _ _) -> id /= requestId)


request : Int -> String -> Drag.File -> FilePortRequest
request =
    FilePortRequest


isImage : FilePortResponse -> Bool
isImage (FilePortResponse (FilePortRequest _ _ { typeMIME }) _) =
    String.startsWith "image" typeMIME


file : FilePortResponse -> Drag.File
file (FilePortResponse (FilePortRequest _ _ file) _) =
    file


base64Encoded : FilePortResponse -> String
base64Encoded (FilePortResponse _ base64Encoded) =
    base64Encoded



---- ENCODING ----


filePortDecoder : List FilePortRequest -> Decode.Decoder FilePortResponse
filePortDecoder requests =
    Decode.field "id" Decode.int
        |> Decode.andThen
            (\requestId ->
                let
                    maybeRequest =
                        requests
                            |> List.filter (\(FilePortRequest id _ _) -> id == requestId)
                            |> List.head
                in
                case maybeRequest of
                    Just request ->
                        Pipeline.decode (FilePortResponse request)
                            |> Pipeline.required "result" Decode.string

                    Nothing ->
                        Decode.fail "Can't find request"
            )
