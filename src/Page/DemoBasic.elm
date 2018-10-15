port module Page.DemoBasic exposing (Model, Msg, init, subscriptions, update, view)

--import Html exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (on, onClick)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import File.Data.UploadId as UploadId exposing (UploadId)
import File.DropZone as DropZone
import File.FileList as FileList
import File.Gallery as Gallery
import File.Upload as Upload
import Html
import Html.Events.Extra.Drag as Drag
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List as FileList
import Task exposing (Task)
import View.Helpers as View



---- PORTS ----


port uploadCmd : Upload.PortCmdMsg -> Cmd msg


port uploadSub : (Upload.PortCmdMsg -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { dropZone : DropZone.State
    , upload : Upload.State
    , files : List Attachment
    , list : FileList.State ()
    }


init : Task Http.Error Model
init =
    Task.map initialModel loadAttachments


initialModel : List Attachment -> Model
initialModel files =
    { dropZone = DropZone.init
    , upload = Upload.init
    , files = files
    , list = FileList.init listConfig
    }



---- CONFIGURATION ----


uploadConfig : Upload.Config Msg
uploadConfig =
    Upload.config NoOp
        --        |> Upload.configMaximumFileSize 2
        |> Upload.configSetStateMsg SetUploadState
        |> Upload.configUploadedMsg Uploaded
        |> Upload.configBase64EncodedMsg EncodeFile
        |> Upload.configAllowedMimeTypes
            [ "image/png"
            , "image/jpeg"
            ]
        |> Upload.configPorts
            { cmd = uploadCmd
            , sub = uploadSub
            }


dropZoneConfig : DropZone.Config Msg
dropZoneConfig =
    DropZone.config NoOp
        |> DropZone.configSetState SetDropZoneState
        |> DropZone.configUploadFiles UploadFiles
        |> DropZone.configBrowseFiles OpenFileBrowser
        --        |> DropZone.configAttrs dropZoneAttrs
        |> DropZone.configContents dropZoneContent
        |> DropZone.configPorts
            { cmd = uploadCmd
            , sub = uploadSub
            }


listConfig : FileList.Config () Attachment Msg
listConfig =
    FileList.config NoOp
        |> FileList.configListStateMsg SetListState
        |> FileList.configCancelUploadMsg CancelUpload
        |> FileList.configIdFn .reference
        |> FileList.configNameFn .fileName
        |> FileList.configContentTypeFn .contentType
        |> FileList.configThumbnailSrcFn (.reference >> (++) "http://localhost:3003/attachments/")



---- ATTACHMENT ----


type alias Attachment =
    { reference : String
    , contentType : String
    , fileName : String
    }


attachmentDecoder : Decode.Decoder Attachment
attachmentDecoder =
    Decode.map3 Attachment
        (Decode.field "reference" Decode.string)
        (Decode.field "mimetype" Decode.string)
        (Decode.field "fileName" Decode.string)


loadAttachments : Task Http.Error (List Attachment)
loadAttachments =
    Http.get "http://localhost:3003/attachments" (Decode.list attachmentDecoder)
        |> Http.toTask



---- UPDATE ----


type Msg
    = NoOp
    | SetDropZoneState DropZone.State
    | SetListState (FileList.State ())
    | SetUploadState Upload.State
    | OpenFileBrowser String
    | UploadFiles (List Drag.File)
    | EncodeFile (Result ( UploadId, String ) ( UploadId, Upload.UploadingFile ))
    | Uploaded (Result ( UploadId, String ) ( UploadId, Encode.Value ))
    | CancelUpload UploadId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUploadState upload ->
            ( { model | upload = upload }
            , Cmd.none
            )

        SetListState list ->
            ( { model | list = list }
            , Cmd.none
            )

        SetDropZoneState dropZone ->
            ( { model | dropZone = dropZone }
            , Cmd.none
            )

        OpenFileBrowser inputID ->
            ( model
            , DropZone.openFileBrowser dropZoneConfig inputID
            )

        UploadFiles files ->
            let
                ( upload, base64Cmd ) =
                    Upload.encode uploadConfig files model.upload
            in
            ( { model | upload = upload }
            , base64Cmd
            )

        EncodeFile (Ok ( id, file )) ->
            let
                updatedUploadState =
                    Upload.update id file model.upload

                uploadUrl =
                    Encode.string "http://localhost:3003/attachments"

                additionalData =
                    Encode.string (Upload.fileFilename file)
            in
            ( { model | upload = updatedUploadState }
            , Upload.upload uploadConfig uploadUrl additionalData id updatedUploadState
            )

        EncodeFile (Err ( uploadId, reason )) ->
            ( { model | upload = Upload.failure uploadId reason model.upload }
            , Cmd.none
            )

        CancelUpload file ->
            model.upload
                |> Upload.cancel uploadConfig file
                |> Tuple.mapFirst (\upload -> { model | upload = upload })

        Uploaded (Ok ( uploadId, encodedAttachment )) ->
            let
                files =
                    encodedAttachment
                        |> Decode.decodeValue attachmentDecoder
                        |> Result.map (\file -> file :: model.files)
                        |> Result.withDefault model.files
            in
            ( { model
                | upload = Upload.success uploadId model.upload
                , files = files
              }
            , Cmd.none
            )

        Uploaded (Err ( uploadId, reason )) ->
            ( { model | upload = Upload.failure uploadId reason model.upload }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    Element.column
        []
        [ Element.el
            [ Border.color (rgb 0 0.7 0)
            ]
            (Element.html (DropZone.view model.dropZone dropZoneConfig))

        --        , Element.html (FileList.view model.list model.upload model.files listConfig)
        ]


dropZoneContent : DropZone.State -> Msg -> List (Html.Attribute Msg) -> List (Html.Html Msg)
dropZoneContent _ openFileBrowser dragAttrs =
    [ layout []
        (row
            (List.concat
                [ List.map htmlAttribute dragAttrs
                , [ height (px 100)
                  , width fill
                  ]
                ]
            )
            [ row
                [ Events.onClick openFileBrowser
                , pointer
                , spacing 5
                , padding 10
                , Border.rounded 5
                , Font.size 22
                , centerX
                , Border.width 1
                , Border.color (rgba 0 0 0 1)
                ]
                [ Element.el [] (View.icon FeatherIcons.upload)
                , Element.el [] (text "Upload")
                ]
            ]
        )
    ]



--view : Model -> Element Msg
--view { upload, files, list, dropZone } =
--    div
--        [ class "container my-4" ]
--        [ div [ class "row" ]
--            [ div [ class "col card" ]
--                [ DropZone.view dropZone dropZoneConfig
--                , FileList.view list upload files listConfig
--                ]
--            ]
--        ]
--
--
--dropZoneContents : DropZone.State -> Msg -> List (Html.Html Msg)
--dropZoneContents _ openFileBrowser =
--    [ h2 [] [ text "Files" ]
--    , span []
--        [ i [ class "fas fa-upload" ] []
--        , text "Drop your files here or "
--        , a [ onClick openFileBrowser ] [ text "browse for a file" ]
--        , text " to upload."
--        ]
--    ]
--
--
--dropZoneAttrs : DropZone.State -> List (Html.Attribute Msg)
--dropZoneAttrs dropzoneState =
--    [ style "width" "100%"
--    , style "height" "150px"
--    , style "border-bottom"
--        (if DropZone.isActive dropzoneState then
--            "2px dashed #ddd"
--
--         else
--            "2px dashed transparent"
--        )
--    , style "background"
--        (if DropZone.isActive dropzoneState then
--            "#dff0d8"
--
--         else
--            "#f7f7f7"
--        )
--    ]
---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Upload.subscriptions uploadConfig model.upload
        , DropZone.subscriptions dropZoneConfig model.dropZone
        ]
