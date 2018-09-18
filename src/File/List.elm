module File.List exposing (view)

import File.File as File
import Html exposing (..)
import Html.Attributes exposing (..)


view : List File.FileReadPortResponse -> Html msg
view files =
    table []
        [ thead []
            [ tr []
                [ th [] []
                , th [] [ text "Filename" ]
                ]
            ]
        , tbody [] (List.map viewRow files)
        ]


viewRow : File.FileReadPortResponse -> Html msg
viewRow response =
    tr []
        [ viewThumbnail response
        , File.file response
            |> .name
            |> text
        ]


viewThumbnail : File.FileReadPortResponse -> Html msg
viewThumbnail file =
    if File.isImage file then
        img [ src (File.base64Encoded file) ] []
    else
        text "non-image thumbnail"
