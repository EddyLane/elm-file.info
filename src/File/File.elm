port module File.File exposing (FilePortRequest, FilePortResponse, encoder, fileContentRead, filePortDecoder, readFileContent, request, requests)

import Date exposing (Date)
import Date.Extra
import Drag
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE


---- PORTS ----


port fileContentRead : (JE.Value -> msg) -> Sub msg


port readFileContent : String -> Cmd msg



---- DATA ----
-- OPAQUE


type FilePortRequest
    = FilePortRequest Int Drag.File


type FilePortResponse
    = FilePortResponse Int FilePortResponseRec


requests : Int -> List Drag.File -> List FilePortRequest
requests requestId files =
    List.indexedMap
        (\i file ->
            request (i + requestId) file
        )
        files


request : Int -> Drag.File -> FilePortRequest
request =
    FilePortRequest



-- RECORDS


type alias FilePortResponseRec =
    { contents : String
    , fileName : String
    }



---- ENCODING ----


filePortDecoder : JD.Decoder FilePortResponse
filePortDecoder =
    JDP.decode FilePortResponse
        |> JDP.required "id" JD.int
        |> JDP.required "data"
            (JDP.decode FilePortResponseRec
                |> JDP.required "contents" JD.string
                |> JDP.required "fileName" JD.string
            )



-- decoder : JD.Decoder FilePortRequest
-- decoder =
--     JDP.decode FilePortRequestRec
--         |> JDP.required "name" JD.string
--         |> JDP.required "type" JD.string
--         |> JDP.required "lastModified" (JD.map Date.fromTime JD.float)
--         |> JDP.optional "tags" (JD.list JD.string) []
--         |> JDP.required "size" JD.int
--         |> JD.andThen (FilePortRequest >> JD.succeed)


encoder : FilePortRequest -> JE.Value
encoder (FilePortRequest id fileData) =
    JE.object
        [ ( "id", JE.int id )
        , ( "data"
          , JE.object
                [ ( "mimeType", JE.string fileData.typeMIME )
                , ( "size", JE.int fileData.size )
                , ( "name", JE.string fileData.name )
                , ( "data", fileData.data )
                ]
          )
        ]
