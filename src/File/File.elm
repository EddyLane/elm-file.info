module File.File exposing (FileData, decoder, encoder)

import Date exposing (Date)
import Date.Extra
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE


type FileData
    = FileData FileDataRec


type alias FileDataRec =
    { name : String
    , type_ : String
    , lastModified : Date
    , tags : List String
    , size : Int
    }


decoder : JD.Decoder FileData
decoder =
    JDP.decode FileDataRec
        |> JDP.required "name" JD.string
        |> JDP.required "type" JD.string
        |> JDP.required "lastModified" (JD.map Date.fromTime JD.float)
        |> JDP.optional "tags" (JD.list JD.string) []
        |> JDP.required "size" JD.int
        |> JD.andThen (FileData >> JD.succeed)


encoder : FileData -> JE.Value
encoder (FileData fileData) =
    JE.object
        [ ( "mimeType", JE.string fileData.type_ )
        , ( "bytes", JE.int fileData.size )
        , ( "filename", JE.string fileData.name )
        , ( "modifiedAt", JE.string (fileData.lastModified |> Date.Extra.toUtcIsoString) )
        , ( "tags", JE.list <| List.map JE.string fileData.tags )
        ]
