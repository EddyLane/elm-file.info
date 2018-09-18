port module File.File
    exposing
        ( FilePortRequest
        , FilePortResponse
        , encoder
        , file
        , fileContentRead
        , filePortDecoder
        , readFileContent
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


port readFileContent : String -> Cmd msg



---- DATA ----
-- OPAQUE


type FilePortRequest
    = FilePortRequest Int String Drag.File


type FilePortResponse
    = FilePortResponse FilePortRequest String


requests : Int -> String -> List Drag.File -> List FilePortRequest
requests requestId inputId =
    List.indexedMap (\i file -> request (i + requestId) inputId file)


removeRequest : FilePortResponse -> List FilePortRequest -> List FilePortRequest
removeRequest (FilePortResponse (FilePortRequest requestId _ _) _) =
    List.filter (\(FilePortRequest id _ _) -> id /= requestId)


request : Int -> String -> Drag.File -> FilePortRequest
request =
    FilePortRequest



--


file : FilePortResponse -> Drag.File
file (FilePortResponse (FilePortRequest _ _ file) _) =
    file



-- RECORDS
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


encoder : FilePortRequest -> Encode.Value
encoder (FilePortRequest id inputId { name }) =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "data"
          , Encode.object
                [ ( "name", Encode.string name )
                , ( "inputId", Encode.string inputId )
                ]
          )
        ]
