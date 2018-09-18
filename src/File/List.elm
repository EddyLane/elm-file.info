module File.List exposing (view)

import File.File as File
import Html exposing (..)
import Html.Attributes exposing (..)


view : List File.FilePortResponse -> Html msg
view files =
    table [ class "s3-files table table-condensed table-files" ]
        [ thead []
            [ tr []
                [ th [] [ text "Filename" ] ]
            ]
        , tbody [] (List.map viewRow files)
        ]


viewRow : File.FilePortResponse -> Html msg
viewRow response =
    tr []
        [ File.file response
            |> .name
            |> text
        ]
