module File.List exposing (view)

import File.File as File
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Context =
    { reading : List File.FileReadPortRequest
    , signing : List File.FileReadPortResponse
    , uploading : List File.FileSigned
    }


view : Context -> Html msg
view context =
    table []
        [ thead []
            [ tr []
                [ th [] []
                , th [] [ text "Filename" ]
                ]
            ]
        , tbody []
            (context
                |> toLifeCycle
                |> List.map viewRow
            )
        ]


toLifeCycle : Context -> List File.FileLifecycle
toLifeCycle { reading, signing, uploading } =
    List.concat
        [ List.map File.lifeCycleReading reading
        , List.map File.lifeCycleSigning signing
        , List.map File.lifeCycleUploading uploading
        ]


viewRow : File.FileLifecycle -> Html msg
viewRow file =
    tr []
        [ viewThumbnail file
        , File.file file
            |> .name
            |> text
        ]


viewThumbnail : File.FileLifecycle -> Html msg
viewThumbnail file =
    img [ src (File.thumbnailSrc file) ] []
