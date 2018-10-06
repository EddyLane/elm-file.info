module Page.Demo exposing (Model, Msg, init, subscriptions, update, view)

import Date exposing (Date)
import Date.Extra
import Drag
import File.Data.SignedUrl as SignedUrl exposing (SignedUrl)
import File.Data.UploadId as UploadId exposing (UploadId)
import File.List as FileList
import File.Upload as Upload
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Task exposing (Task)


signedUrlProviderUrl : String
signedUrlProviderUrl =
    "http://localhost:3003/signed-upload-url"


getAttachmentsUrl : String
getAttachmentsUrl =
    "http://localhost:3003/attachments"


attachmentUrl : Attachment -> String
attachmentUrl { reference } =
    "http://localhost:3003/attachments/" ++ reference



---- MODEL ----


type alias Model =
    { upload : Upload.State Attachment
    , list : FileList.State ColumnId
    , files : List (Taggable Attachment)
    }


init : Task Http.Error Model
init =
    Task.map initialModel loadAttachments


initialModel : List (Taggable Attachment) -> Model
initialModel files =
    { upload = Upload.init
    , list = FileList.init (fileListConfig files)
    , files = files
    }


loadAttachments : Task Http.Error (List (Taggable Attachment))
loadAttachments =
    Http.get getAttachmentsUrl (Decode.list attachmentDecoder)
        |> Http.toTask



{--

A representation of your business logic data type "File" as specified by what you receive from the backend
--}


type Taggable a
    = Taggable Bool a


type alias AttachmentResponse =
    { attachment : Taggable Attachment
    , signedUrl : SignedUrl
    }


type alias Attachment =
    { uploadedAt : Date
    , id : Int
    , reference : String
    , contentType : String
    , fileName : String
    , uploadedBy : String
    , tag : Maybe String
    , softDeleting : Bool
    }


attachmentDecoder : Decode.Decoder (Taggable Attachment)
attachmentDecoder =
    Decode.field "date" Decode.string
        |> Decode.andThen
            (\uploadedAt ->
                case Date.fromString uploadedAt of
                    Ok date ->
                        Pipeline.decode (Attachment date)
                            |> Pipeline.required "id" Decode.int
                            |> Pipeline.required "reference" Decode.string
                            |> Pipeline.required "contentType" Decode.string
                            |> Pipeline.required "fileName" Decode.string
                            |> Pipeline.required "uploadedBy" Decode.string
                            |> Pipeline.required "tag" (Decode.nullable Decode.string)
                            |> Pipeline.required "softDeleting" Decode.bool
                            |> Decode.andThen (Taggable False >> Decode.succeed)

                    Err _ ->
                        Decode.fail "Could not parse date"
            )


attachmentEncoder : Attachment -> Encode.Value
attachmentEncoder attachment =
    Encode.object
        [ ( "id", Encode.int attachment.id )
        , ( "date", Encode.string <| Date.Extra.toIsoString attachment.uploadedAt )
        , ( "reference", Encode.string attachment.reference )
        , ( "contentType", Encode.string attachment.contentType )
        , ( "fileName", Encode.string attachment.fileName )
        , ( "uploadedBy", Encode.string attachment.uploadedBy )
        , ( "softDeleting", Encode.bool attachment.softDeleting )
        , ( "tag"
          , attachment.tag
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        ]



---- UPDATE ----


type Msg
    = NoOp
    | OpenFileBrowser String
    | InputFiles String (List Drag.File)
    | Base64EncodedFile (Result String ( UploadId, Upload.UploadingFile Attachment ))
    | DragFilesOver Drag.Event
    | DragFilesLeave Drag.Event
    | DropFiles Drag.Event
    | GotSignedS3Url UploadId (Result Http.Error AttachmentResponse)
    | UploadedFile (Result String UploadId)
    | UploadFailed UploadId
    | UploadProgress UploadId Float
    | CancelUpload UploadId
    | SetListState (FileList.State ColumnId)
    | ToggleFileTagging Attachment
    | SetTag Attachment (Maybe String)
    | SoftDelete Bool Attachment
    | HardDelete Attachment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragFilesOver _ ->
            ( { model | upload = Upload.dropActive True model.upload }
            , Cmd.none
            )

        DragFilesLeave _ ->
            ( { model | upload = Upload.dropActive False model.upload }
            , Cmd.none
            )

        OpenFileBrowser inputID ->
            ( model
            , Upload.browseClick inputID
            )

        DropFiles { dataTransfer } ->
            let
                ( upload, base64Cmd ) =
                    model.upload
                        |> Upload.dropActive False
                        |> Upload.base64EncodeFiles dataTransfer.files
            in
            ( { model | upload = upload }
            , base64Cmd
            )

        InputFiles _ files ->
            let
                ( upload, base64Cmd ) =
                    model.upload
                        |> Upload.dropActive False
                        |> Upload.base64EncodeFiles files
            in
            ( { model | upload = upload }
            , base64Cmd
            )

        Base64EncodedFile (Ok ( id, file )) ->
            ( { model | upload = Upload.fileReadSuccess id file model.upload }
            , Task.attempt (GotSignedS3Url id) (getSignedUrl file)
            )

        Base64EncodedFile (Err err) ->
            ( model
            , Cmd.none
            )

        GotSignedS3Url uploadId (Ok { attachment, signedUrl }) ->
            let
                (Taggable _ rawAttachment) =
                    attachment

                ( upload, uploadCmd ) =
                    Upload.uploadFileToSignedUrl signedUrl rawAttachment uploadId model.upload
            in
            ( { model | upload = upload }
            , uploadCmd
            )

        GotSignedS3Url uploadId (Err e) ->
            ( { model | upload = Upload.fileUploadFailure uploadId model.upload }
            , Cmd.none
            )

        UploadProgress requestId progress ->
            ( { model | upload = Upload.updateS3UploadProgress requestId progress model.upload }
            , Cmd.none
            )

        CancelUpload file ->
            let
                ( upload, cancelCmd ) =
                    Upload.cancelUpload file model.upload
            in
            ( { model | upload = upload }
            , cancelCmd
            )

        UploadFailed uploadId ->
            ( { model | upload = Upload.fileUploadFailure uploadId model.upload }
            , Cmd.none
            )

        UploadedFile (Ok requestId) ->
            let
                ( upload, maybeAttachment ) =
                    Upload.fileUploadSuccess requestId model.upload

                files =
                    maybeAttachment
                        |> Maybe.map (\attachment -> Taggable False attachment :: model.files)
                        |> Maybe.withDefault model.files
            in
            ( { model
                | upload = upload
                , files = files
              }
            , Cmd.none
            )

        UploadedFile (Err e) ->
            ( model
            , Cmd.none
            )

        ToggleFileTagging target ->
            let
                files =
                    List.map
                        (\((Taggable tagging attachment) as current) ->
                            if attachment.id == target.id then
                                Taggable (not tagging) attachment
                            else
                                current
                        )
                        model.files
            in
            ( { model | files = files }
            , Cmd.none
            )

        SetTag target tag ->
            let
                attachment =
                    { target | tag = tag }

                taggedFile =
                    Taggable False attachment

                files =
                    List.map
                        (\((Taggable _ { reference }) as taggableFile) ->
                            if reference == target.reference then
                                taggedFile
                            else
                                taggableFile
                        )
                        model.files
            in
            ( { model | files = files }
            , Task.attempt (always NoOp) (updateAttachment attachment)
            )

        SetListState list ->
            ( { model | list = list }
            , Cmd.none
            )

        SoftDelete softDeleting target ->
            let
                attachment =
                    { target | softDeleting = softDeleting }

                taggedFile =
                    Taggable False attachment

                files =
                    model.files
                        |> List.map
                            (\((Taggable _ { reference }) as f) ->
                                if target.reference == reference then
                                    taggedFile
                                else
                                    f
                            )
            in
            ( { model | files = files }
            , Task.attempt (always NoOp) (updateAttachment attachment)
            )

        HardDelete target ->
            ( { model | files = List.filter (\(Taggable _ { reference }) -> target.reference /= reference) model.files }
            , Task.attempt (always NoOp) (deleteAttachment target)
            )

        NoOp ->
            ( model, Cmd.none )


getSignedUrl : Upload.UploadingFile file -> Task Http.Error AttachmentResponse
getSignedUrl file =
    Http.post signedUrlProviderUrl
        (Http.jsonBody <| Upload.signedUrlMetadataEncoder file)
        (Pipeline.decode AttachmentResponse
            |> Pipeline.required "attachment" attachmentDecoder
            |> Pipeline.required "signedUrl" SignedUrl.decoder
        )
        |> Http.toTask


updateAttachment : Attachment -> Task Http.Error ()
updateAttachment attachment =
    Http.request
        { method = "PUT"
        , headers = []
        , url = attachmentUrl attachment
        , body = attachmentEncoder attachment |> Http.jsonBody
        , expect = Http.expectStringResponse (always <| Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.toTask


deleteAttachment : Attachment -> Task Http.Error ()
deleteAttachment attachment =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = attachmentUrl attachment
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (always <| Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.toTask



---- VIEW ----


uploadConfig : Upload.Config Msg
uploadConfig =
    Upload.config NoOp
        |> Upload.maximumFileSize 500
        |> Upload.onChangeFiles InputFiles
        |> Upload.browseFiles OpenFileBrowser
        |> Upload.drag DragFilesOver DragFilesLeave DropFiles
        |> Upload.inputId "elm-file-example"
        |> Upload.dropzoneAttrs [ class "card-body" ]


type ColumnId
    = UploadedOn
    | UploadedBy
    | Tags
    | Delete


tags : List String
tags =
    [ "SomeTag"
    , "AnotherTag"
    ]


isCurrentlyTagging : Attachment -> List (Taggable Attachment) -> Bool
isCurrentlyTagging attachment taggableAttachments =
    taggableAttachments
        |> List.filterMap
            (\(Taggable tagging { id }) ->
                if id == attachment.id then
                    Just tagging
                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault False


ifNotSoftDeleting : (( Attachment, Bool ) -> Html Msg) -> ( Attachment, Bool ) -> Html Msg
ifNotSoftDeleting htmlFn ( attachment, isSelected ) =
    if attachment.softDeleting then
        text ""
    else
        htmlFn ( attachment, isSelected )


fileListConfig : List (Taggable Attachment) -> FileList.Config ColumnId Attachment Msg
fileListConfig uploadedFiles =
    FileList.config NoOp
        |> FileList.idFn .reference
        |> FileList.nameFn .fileName
        |> FileList.contentTypeFn .contentType
        |> FileList.thumbnailSrcFn (.reference >> (++) "http://localhost:3003/attachments/")
        |> FileList.cancelUploadMsg CancelUpload
        |> FileList.setListStateMsg SetListState
        |> FileList.defaultSort (FileList.SortByCustom UploadedOn)
        |> FileList.defaultSortDirection FileList.Desc
        |> FileList.rowDisabled .softDeleting
        |> FileList.multiSelectEnabled False
        |> FileList.failedRowAttrs (always [ style [ ( "background-color", "#f8d7da" ) ] ])
        |> FileList.fileIcon
            (\contentType ->
                let
                    fileType =
                        contentType
                            |> String.split "/"
                            |> List.reverse
                            |> List.head
                in
                span
                    [ class "h3 m-0 p-0 far"
                    , class
                        (case fileType of
                            Just "pdf" ->
                                "fa-file-pdf"

                            Just "word" ->
                                "fa-file-word"

                            _ ->
                                "fa-file-alt"
                        )
                    ]
                    []
            )
        |> FileList.uploadedRowAttrs
            (\attachment ->
                [ style
                    [ ( "background-color"
                      , if attachment.softDeleting then
                            "#f8d7da"
                        else
                            "#fff"
                      )
                    ]
                ]
            )
        |> FileList.rowActions
            (\attachment ->
                if isCurrentlyTagging attachment uploadedFiles then
                    let
                        decoder =
                            Decode.map (SetTag attachment) <|
                                Decode.andThen (Just >> Decode.succeed) targetValue
                    in
                    Just <|
                        div []
                            [ select
                                [ class "form-control form-control-lg"
                                , on "change" decoder
                                ]
                                (List.map
                                    (\t ->
                                        option
                                            [ value t
                                            , selected
                                                (attachment.tag
                                                    |> Maybe.map ((==) t)
                                                    |> Maybe.withDefault False
                                                )
                                            ]
                                            [ text t ]
                                    )
                                    tags
                                )
                            , button
                                [ class "btn btn-link"
                                , onClick (ToggleFileTagging attachment)
                                ]
                                [ text "Close" ]
                            ]
                else
                    Nothing
            )
        |> FileList.column
            { id = UploadedOn
            , label = "Uploaded on"
            , html = ifNotSoftDeleting (Tuple.first >> .uploadedAt >> Date.Extra.toFormattedString "d MMM YYY HH:mm" >> text)
            , sorter = Just <| \a b -> Date.Extra.compare a.uploadedAt b.uploadedAt
            }
        |> FileList.column
            { id = UploadedBy
            , label = "Uploaded by"
            , html = ifNotSoftDeleting (Tuple.first >> .uploadedBy >> text)
            , sorter = Just <| \a b -> compare a.uploadedBy b.uploadedBy
            }
        |> FileList.column
            { id = Tags
            , label = "Tag"
            , html =
                ifNotSoftDeleting
                    (\( attachment, isSelected ) ->
                        case attachment.tag of
                            Just tag ->
                                div [ class "badge badge-secondary" ]
                                    [ text tag
                                    , button
                                        [ class "btn btn-sm btn-link p-0 ml-1"
                                        , onClick (SetTag attachment Nothing)
                                        ]
                                        [ span [ class "text-white" ] [ text "×" ]
                                        ]
                                    ]

                            Nothing ->
                                button
                                    [ class "btn btn-link "
                                    , onClick (ToggleFileTagging attachment)
                                    ]
                                    [ text
                                        (if isCurrentlyTagging attachment uploadedFiles then
                                            "Cancel"
                                         else
                                            "Add tag"
                                        )
                                    ]
                    )
            , sorter =
                Just <|
                    \a b ->
                        case ( a.tag, b.tag ) of
                            ( Just a, Just b ) ->
                                compare a b

                            ( Just a, Nothing ) ->
                                compare 1 2

                            ( Nothing, Just b ) ->
                                compare 2 1

                            ( Nothing, Nothing ) ->
                                compare 1 1
            }
        |> FileList.column
            { id = Delete
            , label = "Delete"
            , html =
                \( attachment, _ ) ->
                    if not attachment.softDeleting then
                        button
                            [ onClick (SoftDelete True attachment)
                            , class "btn btn-sm btn-outline-danger"
                            ]
                            [ text "Delete" ]
                    else
                        div [ class "btn-toolbar" ]
                            [ div [ class "input-group mr-1" ]
                                [ button
                                    [ onClick (SoftDelete False attachment)
                                    , class "btn btn-sm btn-outline-secondary"
                                    ]
                                    [ text "Cancel" ]
                                ]
                            , div [ class "input-group" ]
                                [ button
                                    [ onClick (HardDelete attachment)
                                    , class "btn btn-sm btn-danger"
                                    ]
                                    [ text "Confirm" ]
                                ]
                            ]
            , sorter = Nothing
            }


view : Model -> Html Msg
view { upload, files, list } =
    let
        attachments =
            List.map (\(Taggable _ attachment) -> attachment) files
    in
    div
        [ class "container my-4" ]
        [ div [ class "row" ]
            [ div [ class "col card" ]
                [ Upload.view upload uploadConfig ]
            ]
        , div [ class "row mt-4" ]
            [ div [ class "col card" ]
                [ FileList.view list upload attachments (fileListConfig files) ]
            ]
        ]



---- SUBSCRIPTIONS ----


base64EncodeFileSub : Upload.State file -> Sub (Result String ( UploadId, Upload.UploadingFile file ))
base64EncodeFileSub upload =
    Upload.fileContentRead (Decode.decodeValue <| Upload.base64PortDecoder upload)


fileUploadedSub : Sub Msg
fileUploadedSub =
    Upload.uploaded (Decode.decodeValue UploadId.decoder >> UploadedFile)


fileFailureSub : Sub Msg
fileFailureSub =
    Upload.uploadFailed
        (Decode.decodeValue UploadId.decoder
            >> Result.toMaybe
            >> Maybe.map UploadFailed
            >> Maybe.withDefault NoOp
        )


fileUploadProgressSub : Sub Msg
fileUploadProgressSub =
    Upload.uploadProgress
        (\( id, progress ) ->
            case Decode.decodeValue UploadId.decoder id of
                Ok uploadId ->
                    UploadProgress uploadId progress

                Err _ ->
                    NoOp
        )


subscriptions : Model -> Sub Msg
subscriptions { upload } =
    Sub.batch
        [ Sub.map Base64EncodedFile (base64EncodeFileSub upload)
        , fileUploadedSub
        , fileFailureSub
        , fileUploadProgressSub
        ]
