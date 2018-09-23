module File.List exposing (view)

import File.File as File
import File.Upload as Upload
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


view : Upload.Config msg file -> Upload.State file -> List file -> Html msg
view config upload files =
    text ""



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
--viewThumbnail : Upload.Config msg file -> Upload.UploadState file -> Html msg
--viewThumbnail config file =
--    img [ src (Upload.thumbnailSrc config file) ] []
