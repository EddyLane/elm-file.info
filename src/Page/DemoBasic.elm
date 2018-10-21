port module Page.DemoBasic exposing (Model, Msg, init, subscriptions, update, view)

import Animation
import Css exposing (..)
import FeatherIcons
import File.Data.UploadId as UploadId exposing (UploadId)
import File.DropZone as DropZone
import File.FileList as FileList
import File.Gallery as Gallery
import File.Upload as Upload
import Html
import Html.Events exposing (on, onClick)
import Html.Events.Extra.Drag as Drag
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, src, style)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List as FileList
import Task exposing (Task)



---- PORTS ----


port uploadCmd : Upload.PortCmdMsg -> Cmd msg


port uploadSub : (Upload.PortCmdMsg -> msg) -> Sub msg


port galleryCmd : Gallery.PortCmdMsg -> Cmd msg


port gallerySub : (Gallery.PortCmdMsg -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { dropZone : DropZone.State
    , upload : Upload.State
    , files : List Attachment
    , list : FileList.State ()
    , gallery : Gallery.State
    , test : Animation.State
    }


init : Task Http.Error ( Model, Cmd Msg )
init =
    Task.map initialModel loadAttachments
        |> Task.andThen
            (\model ->
                Task.succeed <|
                    ( model
                    , Gallery.getBoundingClientRects galleryConfig model.files model.upload
                    )
            )


initialModel : List Attachment -> Model
initialModel files =
    { dropZone = DropZone.init
    , upload = Upload.init
    , files = files
    , list = FileList.init listConfig
    , gallery = Gallery.init
    , test = Animation.style []
    }



---- CONFIGURATION ----


uploadConfig : Upload.Config Msg
uploadConfig =
    Upload.config NoOp
        |> Upload.configMaximumFileSize 100000000
        |> Upload.configSetStateMsg SetUploadState
        |> Upload.configUploadedMsg Uploaded
        |> Upload.configBase64EncodedMsg EncodeFile
        --        |> Upload.configAllowedMimeTypes
        --            [ "image/png"
        --            , "image/jpeg"
        --            ]
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
        |> DropZone.configContents
            (\_ openFileBrowser ->
                [ button
                    [ onClick openFileBrowser
                    , css
                        [ width (pct 100)
                        , height (pct 100)
                        , position absolute
                        , left (px 0)
                        , top (px 0)
                        , cursor pointer
                        , backgroundColor inherit
                        , border (px 0)
                        , color (hex "fff")
                        ]
                    ]
                    [ FeatherIcons.plus
                        |> FeatherIcons.withSizeUnit "%"
                        |> FeatherIcons.withSize 50
                        |> FeatherIcons.toHtml []
                        |> fromUnstyled
                    ]
                ]
            )
        |> DropZone.configAttrs (always [])
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


galleryConfig : Gallery.Config Attachment Msg
galleryConfig =
    Gallery.config NoOp
        |> Gallery.configAnimationMsg GalleryAnimation
        |> Gallery.configSetState SetGalleryState
        |> Gallery.configCancelUploadMsg CancelUpload
        |> Gallery.configIdFn .reference
        |> Gallery.configNameFn .fileName
        |> Gallery.configContentTypeFn .contentType
        |> Gallery.configThumbnailSrcFn (.reference >> (++) "http://localhost:3003/attachments/")
        |> Gallery.configPorts
            { cmd = galleryCmd
            , sub = gallerySub
            }



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
    | SetGalleryState Gallery.State
    | SetUploadState Upload.State
    | OpenFileBrowser String
    | UploadFiles (List Drag.File)
    | EncodeFile (Result ( UploadId, String ) ( UploadId, Upload.UploadingFile ))
    | Uploaded (Result ( UploadId, String ) ( UploadId, Encode.Value ))
    | CancelUpload UploadId
    | GalleryAnimation String Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGalleryState gallery ->
            ( { model | gallery = gallery }
            , Cmd.none
            )

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

                upload =
                    Upload.success uploadId model.upload
            in
            ( { model
                | upload = upload
                , files = files
              }
            , Gallery.getBoundingClientRects galleryConfig files upload
            )

        Uploaded (Err ( uploadId, reason )) ->
            ( { model | upload = Upload.failure uploadId reason model.upload }
            , Cmd.none
            )

        GalleryAnimation elementId animMsg ->
            ( { model | gallery = Gallery.animate animMsg elementId model.gallery }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { upload, files, list, dropZone, gallery } =
    let
        dropZoneView =
            DropZone.view dropZone dropZoneConfig
    in
    div
        [ css
            [ backgroundImage (linearGradient (stop <| hex "444444") (stop <| hex "666666") [])
            , width (pct 100)
            , height (pct 100)
            ]
        ]
        [ Gallery.view (Just dropZoneView) gallery files upload galleryConfig
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Upload.subscriptions uploadConfig model.upload
        , DropZone.subscriptions dropZoneConfig model.dropZone
        , Gallery.subscriptions galleryConfig model.gallery
        ]
