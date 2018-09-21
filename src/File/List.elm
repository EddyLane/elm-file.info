module File.List exposing (view)

import File.File as File
import File.Upload as Upload
import Html exposing (..)
import Html.Attributes exposing (..)


view : Upload.Config msg file -> Upload.State file -> List file -> Html msg
view config upload files =
    table []
        [ thead []
            [ tr []
                [ th [] []
                , th [] [ text "Filename" ]
                , th [] [ text "Progress" ]
                ]
            ]
        , tbody []
            (files
                |> Upload.files upload
                |> List.map (viewRow config)
            )
        ]


viewRow : Upload.Config msg file -> Upload.UploadState file -> Html msg
viewRow config file =
    tr []
        [ th [] [ viewThumbnail config file ]
        , th [] [ text <| Upload.fileName config file ]
        , th [] [ text <| toString (Upload.uploadPercentage file) ]
        ]


viewThumbnail : Upload.Config msg file -> Upload.UploadState file -> Html msg
viewThumbnail config file =
    img [ src (Upload.thumbnailSrc config file) ] []
