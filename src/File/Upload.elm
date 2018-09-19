port module File.Upload
    exposing
        ( Config
        , State
        , backendUrl
        , browseClick
        , browseFiles
        , config
        , drag
        , dropActive
        , getInputId
        , getRequestId
        , init
        , inputId
        , maximumFileSize
        , onChangeFiles
        , updateRequestId
        , uploadFile
        , view
        )

import Drag
import File.File as File
import File.SignedUrl as SignedUrl
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onWithOptions)
import Json.Decode as Decode
import Json.Encode as Encode
import Mouse


--- PORTS -----


port browseClick : String -> Cmd msg



---- STATE ----


type State
    = State StateRec


type alias StateRec =
    { dropActive : Bool
    , selectAllToggled : Bool
    , requestId : Int
    }


type Config msg
    = Config (ConfigRec msg)


type alias ConfigRec msg =
    { onChangeFilesMsg : String -> List Drag.File -> msg
    , uploadFileMsg : File.FileReadPortRequest -> msg
    , softDeleteSelectedMsg : msg
    , browseClickMsg : String -> msg
    , dragOverMsg : Drag.Event -> msg
    , dragLeaveMsg : Drag.Event -> msg
    , dropMsg : Drag.Event -> msg
    , maximumFileSize : Int
    , backendUrl : String
    , inputId : String
    }


init : State
init =
    State <|
        { dropActive = False
        , selectAllToggled = False
        , requestId = 0
        }


config : msg -> Config msg
config noOpMsg =
    Config <|
        { onChangeFilesMsg = always (always noOpMsg)
        , softDeleteSelectedMsg = noOpMsg
        , browseClickMsg = always noOpMsg
        , dragOverMsg = always noOpMsg
        , dragLeaveMsg = always noOpMsg
        , dropMsg = always noOpMsg
        , uploadFileMsg = always noOpMsg
        , maximumFileSize = 5000
        , backendUrl = ""
        , inputId = "elm-file-upload-input"
        }


onChangeFiles : (String -> List Drag.File -> msg) -> Config msg -> Config msg
onChangeFiles msg (Config configRec) =
    Config <|
        { configRec | onChangeFilesMsg = msg }


inputId : String -> Config msg -> Config msg
inputId inputId (Config configRec) =
    Config <|
        { configRec | inputId = inputId }


getInputId : Config msg -> String
getInputId (Config { inputId }) =
    inputId


getRequestId : State -> Int
getRequestId (State { requestId }) =
    requestId


softDelete : msg -> Config msg -> Config msg
softDelete msg (Config configRec) =
    Config <|
        { configRec | softDeleteSelectedMsg = msg }


browseFiles : (String -> msg) -> Config msg -> Config msg
browseFiles msg (Config configRec) =
    Config <|
        { configRec | browseClickMsg = msg }


drag : (Drag.Event -> msg) -> (Drag.Event -> msg) -> (Drag.Event -> msg) -> Config msg -> Config msg
drag over leave drop (Config configRec) =
    Config <|
        { configRec
            | dragOverMsg = over
            , dragLeaveMsg = leave
            , dropMsg = drop
        }


maximumFileSize : Int -> Config msg -> Config msg
maximumFileSize size (Config configRec) =
    Config <|
        { configRec | maximumFileSize = size }


uploadFile : (File.FileReadPortRequest -> msg) -> Config msg -> Config msg
uploadFile msg (Config configRec) =
    Config <|
        { configRec | uploadFileMsg = msg }


backendUrl : String -> Config msg -> Config msg
backendUrl backendUrl (Config configRec) =
    Config <|
        { configRec | backendUrl = backendUrl }



---- UPDATE ----


dropActive : Bool -> State -> State
dropActive isActive (State state) =
    State { state | dropActive = isActive }


updateRequestId : Int -> State -> State
updateRequestId addedRequestAmount (State state) =
    State { state | requestId = state.requestId + addedRequestAmount }



-- onChangeFiles : List Drag.File
{- -}
-- fileChanges : List Drag.File -> List ( File.FileReadPortRequest, Decode.Value )
-- fileChanges files =
--     List.filterMap decodeData files
-- decodeData : Drag.File -> Maybe ( File.FileReadPortRequest, Decode.Value )
-- decodeData { data } =
--     data
--         |> Decode.decodeValue File.decoder
--         |> Result.toMaybe
--         |> Maybe.map (\x -> ( x, data ))
-- upload : Config msg -> File.FileReadPortRequest -> String -> msg
-- upload config FileReadPortRequest requestId =
--     let
--         payload =
--             File.encoder FileReadPortRequest
--     in
--     Cmd.none
-- requestAttachmentsInsert : String -> JE.Value -> (APIData Types.AttachmentWithUsername -> msg) -> Cmd msg
-- requestAttachmentsInsert backendUrl payload msg =
--     let
--         apiCallCmd =
--             Push.init "service:all" "request:attachments:insert"
--                 |> Push.withPayload payload
--                 |> Push.onOk (okHandler msg <| Decoders.attachmentWithUsername)
--                 |> Push.onError (errorHandler msg)
--                 |> Phoenix.push (socketUrl backendUrl)
--     in
--     Cmd.batch [ progressCmd msg, apiCallCmd ]
-- API.requestAttachmentsInsert model.backendUrl payload (InsertResult FileReadPortRequestV)
---- VIEW ----


descriptions : List String
descriptions =
    [ "Drawings"
    , "Technical Data"
    , "Photos"
    , "Legal"
    , "Contracts"
    , "Financial"
    , "Insurance"
    ]


view : State -> Config msg -> Html msg
view (State state) (Config config) =
    div []
        [ dropZone state config
        , fileInput config
        ]


dropZone : StateRec -> ConfigRec msg -> Html msg
dropZone state config =
    div
        [ style
            [ ( "width", "100%" )
            , ( "height", "150px" )
            , ( "border-bottom"
              , if state.dropActive then
                    "2px dashed #ddd"
                else
                    "2px dashed transparent"
              )
            , ( "background"
              , if state.dropActive then
                    "#dff0d8"
                else
                    "#f7f7f7"
              )
            ]
        , Drag.onOver config.dragOverMsg
        , Drag.onLeave config.dragLeaveMsg
        , Drag.onDrop config.dropMsg
        ]
    <|
        [ div [] [ text "Files" ]
        , p [ class "upload-file-container" ]
            [ i [ class "fa fa-upload" ] []
            , text "Drop your files here or "
            , a
                [ href "javascript:void(0)"
                , onClick (config.browseClickMsg config.inputId)
                ]
                [ text "browse for a file" ]
            , text " to upload."
            ]
        ]


fileInput : ConfigRec msg -> Html msg
fileInput { inputId, onChangeFilesMsg } =
    input
        [ style [ ( "display", "none" ) ]
        , attribute "multiple" ""
        , type_ "file"
        , id inputId
        , onWithOptions
            "change"
            { stopPropagation = False
            , preventDefault = False
            }
            (fileInputDecoder inputId onChangeFilesMsg)
        ]
        []


fileInputDecoder : String -> (String -> List Drag.File -> msg) -> Decode.Decoder msg
fileInputDecoder inputId msg =
    Drag.fileDecoder
        |> Drag.fileListDecoder
        |> Decode.at [ "target", "files" ]
        |> Decode.map (msg inputId)
