module File.Upload
    exposing
        ( Config
        , State
        , backendUrl
        , browseFiles
        , config
        , init
        , maximumFileSize
        , onChangeFiles
        , update
        , uploadFile
        , view
        )

import Drag
import File.File as File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onWithOptions)
import Json.Decode as Decode


---- STATE ----


type alias AttachmentData a =
    { selected : Bool
    , editMode : Bool
    , attachment : a
    }


type State
    = State StateRec


type alias StateRec =
    { dropActive : Bool
    , selectAllToggled : Bool
    }


type Config msg
    = Config (ConfigRec msg)


type alias ConfigRec msg =
    { onChangeFilesMsg : List Drag.File -> msg
    , uploadFileMsg : File.FilePortRequest -> msg
    , softDeleteSelectedMsg : msg
    , browseClickMsg : msg
    , dragOverMsg : Drag.Event -> msg
    , dragLeaveMsg : Drag.Event -> msg
    , dropMsg : Drag.Event -> msg
    , maximumFileSize : Int
    , backendUrl : String
    }


init : State
init =
    State <|
        { dropActive = False
        , selectAllToggled = False
        }


config : msg -> Config msg
config noOpMsg =
    Config <|
        { onChangeFilesMsg = always noOpMsg
        , softDeleteSelectedMsg = noOpMsg
        , browseClickMsg = noOpMsg
        , dragOverMsg = always noOpMsg
        , dragLeaveMsg = always noOpMsg
        , dropMsg = always noOpMsg
        , uploadFileMsg = always noOpMsg
        , maximumFileSize = 5000
        , backendUrl = ""
        }


onChangeFiles : (List Drag.File -> msg) -> Config msg -> Config msg
onChangeFiles msg (Config configRec) =
    Config <|
        { configRec | onChangeFilesMsg = msg }


softDelete : msg -> Config msg -> Config msg
softDelete msg (Config configRec) =
    Config <|
        { configRec | softDeleteSelectedMsg = msg }


browseFiles : msg -> Config msg -> Config msg
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


uploadFile : (File.FilePortRequest -> msg) -> Config msg -> Config msg
uploadFile msg (Config configRec) =
    Config <|
        { configRec | uploadFileMsg = msg }


backendUrl : String -> Config msg -> Config msg
backendUrl backendUrl (Config configRec) =
    Config <|
        { configRec | backendUrl = backendUrl }



---- UPDATE ----


update : { a | upload : State } -> (State -> ( State, Cmd msg )) -> ( { a | upload : State }, Cmd msg )
update model updateFn =
    let
        ( upload, cmd ) =
            updateFn model.upload
    in
    ( { model | upload = upload }
    , cmd
    )



-- onChangeFiles : List Drag.File
{- -}
-- fileChanges : List Drag.File -> List ( File.FilePortRequest, Decode.Value )
-- fileChanges files =
--     List.filterMap decodeData files
-- decodeData : Drag.File -> Maybe ( File.FilePortRequest, Decode.Value )
-- decodeData { data } =
--     data
--         |> Decode.decodeValue File.decoder
--         |> Result.toMaybe
--         |> Maybe.map (\x -> ( x, data ))
-- upload : Config msg -> File.FilePortRequest -> String -> msg
-- upload config FilePortRequest requestId =
--     let
--         payload =
--             File.encoder FilePortRequest
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
-- API.requestAttachmentsInsert model.backendUrl payload (InsertResult FilePortRequestV)
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
    let
        -- hasChecked =
        --     state.attachments
        --         |> RemoteData.map (List.map .selected >> List.any (\x -> x == True))
        --         |> RemoteData.withDefault False
        onOptions =
            { stopPropagation = False
            , preventDefault = False
            }
    in
    div [ class "tile tile-attachments s3s-files" ]
        [ div
            [ classList
                [ ( "drop-zone", True )
                , ( "drop-active", state.dropActive )
                ]
            , Drag.onOver config.dragOverMsg
            , Drag.onLeave config.dragLeaveMsg
            , Drag.onDrop config.dropMsg
            ]
          <|
            List.concat
                [ [ div [ class "tile-hd" ]
                        [ div [ class "tile-name" ]
                            [ text "Files" ]
                        ]
                  , p [ class "upload-file-container" ]
                        [ i [ class "fa fa-upload" ]
                            []
                        , text "Drop your files here or "
                        , a
                            [ attribute "data-action" "choose-files"
                            , href "javascript:void(0)"
                            , onClick config.browseClickMsg
                            ]
                            [ text "browse for a file " ]
                        , text "to upload."
                        ]
                  ]
                , []
                , []
                , [ Html.form [ id "file-upload-form" ]
                        [ input
                            [ attribute "multiple" ""
                            , type_ "file"
                            , id "file-upload-input"
                            , onWithOptions "change" onOptions <|
                                Decode.map config.onChangeFilesMsg <|
                                    Decode.at [ "target", "files" ] <|
                                        Drag.fileListDecoder Drag.fileDecoder
                            ]
                            []
                        ]
                  ]
                ]
        ]
