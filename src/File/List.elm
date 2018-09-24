module File.List
    exposing
        ( Config
        , config
        , contentTypeFn
        , nameFn
        , thumbnailSrcFn
        , view
        )

import File.Base64Encoded as Base64Encoded exposing (Base64Encoded)
import File.Upload as Upload exposing (UploadingFile)
import File.UploadId as UploadId exposing (Collection, UploadId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type UploadState file
    = Uploading UploadId (UploadingFile file)
    | Uploaded file


type Config file
    = Config (ConfigRec file)


type alias ConfigRec file =
    { nameFn : file -> String
    , contentTypeFn : file -> String
    , thumbnailSrcFn : file -> String
    }



---- CONFIG ----


config : Config file
config =
    Config <|
        { nameFn = always "-"
        , contentTypeFn = always "-"
        , thumbnailSrcFn = always ""
        }


nameFn : (file -> String) -> Config file -> Config file
nameFn fn (Config configRec) =
    Config <|
        { configRec | nameFn = fn }


contentTypeFn : (file -> String) -> Config file -> Config file
contentTypeFn fn (Config configRec) =
    Config <|
        { configRec | contentTypeFn = fn }


thumbnailSrcFn : (file -> String) -> Config file -> Config file
thumbnailSrcFn fn (Config configRec) =
    Config <|
        { configRec | thumbnailSrcFn = fn }


combineUploadsWithFiles : List file -> UploadId.Collection (Upload.UploadingFile file) -> List (UploadState file)
combineUploadsWithFiles files uploads =
    List.concat
        [ List.map (uncurry Uploading) <| UploadId.toList uploads
        , List.map Uploaded files
        ]


fileName : Config file -> UploadState file -> String
fileName (Config { nameFn }) file =
    case file of
        Uploading _ file ->
            Upload.fileName file

        Uploaded uploaded ->
            nameFn uploaded


thumbnailSrc : Config file -> UploadState file -> String
thumbnailSrc (Config { thumbnailSrcFn, contentTypeFn }) file =
    case ( isImage contentTypeFn file, file ) of
        ( True, Uploading _ file ) ->
            file
                |> Upload.base64EncodedData
                |> Maybe.map Base64Encoded.toString
                |> Maybe.withDefault ""

        ( True, Uploaded file ) ->
            thumbnailSrcFn file

        _ ->
            ""


uploadPercentage : UploadState file -> Float
uploadPercentage file =
    case file of
        Uploading _ file ->
            Upload.uploadPercentage file

        Uploaded _ ->
            100.0


isImage : (file -> String) -> UploadState file -> Bool
isImage contentTypeFn file =
    case file of
        Uploading _ file ->
            Upload.isImage file

        Uploaded backendFile ->
            String.startsWith "image" (contentTypeFn backendFile)


view : Config file -> Upload.State file -> List file -> Html msg
view config upload files =
    table []
        [ thead []
            [ tr []
                [ th [] []
                , th [] [ text "Filename" ]
                , th [] [ text "Progress" ]
                , th [] []
                ]
            ]
        , tbody []
            (upload
                |> Upload.uploads
                |> combineUploadsWithFiles files
                |> List.map (always <| text "")
            )
        ]



--viewRow : Config file -> UploadState file -> Html msg
--viewRow config file =
--    tr []
--        [ td [] [ viewThumbnail config file ]
----        , td [] [ text <| Upload.fileName config file ]
----        , td [] [ text <| toString (Upload.uploadPercentage file) ]
----        , td [] [ button [ onClick (Upload.cancelTrigger config file) ] [ text "Cancel" ] ]
--        ]
--
--viewThumbnail : Config msg file -> UploadState file -> Html msg
--viewThumbnail config file =
--    img [ src (thumbnailSrc config file) ] []
--view : Upload.Config msg file -> Upload.State file -> List file -> Html msg
--view config upload files =
--    table []
--        [ thead []
--            [ tr []
--                [ th [] []
--                , th [] [ text "Filename" ]
--                , th [] [ text "Progress" ]
--                , th [] []
--                ]
--            ]
--        , tbody []
--            (files
--                |> Upload.files upload
--                |> List.map (viewRow config)
--            )
--        ]
--
--viewRow : Upload.Config msg file -> Upload.UploadState file -> Html msg
--viewRow config file =
--    tr []
--        [ td [] [ viewThumbnail config file ]
--        , td [] [ text <| Upload.fileName config file ]
--        , td [] [ text <| toString (Upload.uploadPercentage file) ]
--        , td [] [ button [ onClick (Upload.cancelTrigger config file) ] [ text "Cancel" ] ]
--        ]
--
--
