module File.List exposing (view)

import File.File as File
import File.Upload as Upload
import Html exposing (..)
import Html.Attributes exposing (..)


view : Upload.State -> Html msg
view upload =
    table []
        [ thead []
            [ tr []
                [ th [] []
                , th [] [ text "Filename" ]
                , th [] [ text "Progress" ]
                ]
            ]
        , tbody []
            (upload
                |> Upload.files
                |> List.map viewRow
            )
        ]


viewRow : File.FileLifecycle -> Html msg
viewRow file =
    tr []
        [ th [] [ viewThumbnail file ]
        , th [] [ file |> File.file |> .name |> text ]
        , th [] [ file |> File.uploadProgress |> toString |> text ]
        ]


viewThumbnail : File.FileLifecycle -> Html msg
viewThumbnail file =
    img [ src (File.thumbnailSrc file) ] []
