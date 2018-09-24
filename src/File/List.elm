module File.List
    exposing
        ( Config
        , State
        , cancelUploadMsg
        , column
        , columns
        , config
        , contentTypeFn
        , init
        , nameFn
        , setListStateMsg
        , thumbnailSrcFn
        , view
        )

import File.Base64Encoded as Base64Encoded exposing (Base64Encoded)
import File.Upload as Upload exposing (UploadingFile)
import File.UploadId as UploadId exposing (Collection, UploadId)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onClick)


type UploadState file
    = Uploading UploadId (UploadingFile file)
    | Uploaded file


type Config file msg
    = Config (ConfigRec file msg)


type alias ConfigRec file msg =
    { nameFn : file -> String
    , contentTypeFn : file -> String
    , thumbnailSrcFn : file -> String
    , cancelUploadMsg : UploadId -> msg
    , columns : List (Column file msg)
    , setListStateMsg : State -> msg
    }


type State
    = State SortDirection SortableColumn


type SortDirection
    = Asc
    | Desc


type SortableColumn
    = File
    | Custom String


type alias Column file msg =
    { label : String
    , html : file -> Html msg
    , sorter : file -> file -> Order
    }


init : State
init =
    State Asc File



---- CONFIG ----


config : msg -> Config file msg
config noOpMsg =
    Config <|
        { nameFn = always "-"
        , contentTypeFn = always "-"
        , cancelUploadMsg = always noOpMsg
        , thumbnailSrcFn = always ""
        , columns = []
        , setListStateMsg = always noOpMsg
        }


setListStateMsg : (State -> msg) -> Config file msg -> Config file msg
setListStateMsg msg (Config configRec) =
    Config <|
        { configRec | setListStateMsg = msg }


nameFn : (file -> String) -> Config file msg -> Config file msg
nameFn fn (Config configRec) =
    Config <|
        { configRec | nameFn = fn }


contentTypeFn : (file -> String) -> Config file msg -> Config file msg
contentTypeFn fn (Config configRec) =
    Config <|
        { configRec | contentTypeFn = fn }


thumbnailSrcFn : (file -> String) -> Config file msg -> Config file msg
thumbnailSrcFn fn (Config configRec) =
    Config <|
        { configRec | thumbnailSrcFn = fn }


cancelUploadMsg : (UploadId -> msg) -> Config file msg -> Config file msg
cancelUploadMsg msg (Config configRec) =
    Config <|
        { configRec | cancelUploadMsg = msg }


columns : List (Column file msg) -> Config file msg -> Config file msg
columns columns (Config configRec) =
    Config <|
        { configRec | columns = columns }


column : Column file msg -> Config file msg -> Config file msg
column col (Config configRec) =
    Config <|
        { configRec | columns = col :: configRec.columns }



---- VIEW ----


view : State -> Config file msg -> Upload.State file -> List file -> Html msg
view ((State sortDir sortCol) as state) config upload files =
    table []
        [ viewTableHeader state config
        , tbody []
            (upload
                |> Upload.uploads
                |> combineUploadsWithFiles files
                |> sortResults config state
                |> List.map (viewRow config)
            )
        ]


sortResults : Config file msg -> State -> List (UploadState file) -> List (UploadState file)
sortResults config (State sortDir sortCol) =
    case sortCol of
        File ->
            List.sortWith (sortByFilename config sortDir)

        Custom headerName ->
            List.sortWith (sortByCustom headerName config sortDir)


sortByCustom : String -> Config file msg -> SortDirection -> UploadState file -> UploadState file -> Order
sortByCustom headerName ((Config { columns }) as config) sortDir a b =
    columns
        |> List.filter (\{ label } -> label == headerName)
        |> List.head
        |> Maybe.map
            (\{ sorter } ->
                case sortDirPair sortDir a b of
                    ( Uploaded a, Uploaded b ) ->
                        sorter a b

                    ( Uploading _ a, Uploaded b ) ->
                        Basics.compare 1 2

                    ( Uploaded a, Uploading _ b ) ->
                        Basics.compare 2 1

                    ( Uploading _ a, Uploading _ b ) ->
                        Basics.compare 1 1
            )
        |> Maybe.withDefault (sortByFilename config sortDir a b)


sortByFilename : Config file msg -> SortDirection -> UploadState file -> UploadState file -> Order
sortByFilename (Config { nameFn }) sortDir a b =
    case sortDirPair sortDir a b of
        ( Uploaded a, Uploaded b ) ->
            Basics.compare (nameFn a) (nameFn b)

        ( Uploading _ a, Uploaded b ) ->
            Basics.compare (Upload.fileName a) (nameFn b)

        ( Uploaded a, Uploading _ b ) ->
            Basics.compare (nameFn a) (Upload.fileName b)

        ( Uploading _ a, Uploading _ b ) ->
            Basics.compare (Upload.fileName a) (Upload.fileName b)


sortDirPair : SortDirection -> a -> a -> ( a, a )
sortDirPair sortDir a b =
    if sortDir == Asc then
        ( a, b )
    else
        ( b, a )


viewTableHeader : State -> Config file msg -> Html msg
viewTableHeader state ((Config { columns, setListStateMsg }) as config) =
    thead []
        [ tr []
            (List.concat
                [ [ th [] [ text "" ]
                  , th [] [ text "Thumbnail" ]
                  , th [ onClick (setListStateMsg (viewListSorterState File state)) ] [ text "File" ]
                  ]
                , viewUserDefinedThs state config
                , [ th [] [ text "" ] ]
                ]
            )
        ]


viewUserDefinedThs : State -> Config file msg -> List (Html msg)
viewUserDefinedThs state (Config { columns, setListStateMsg }) =
    List.map
        (\{ label } ->
            th [ onClick <| setListStateMsg <| viewListSorterState (Custom label) state ] [ text label ]
        )
        columns


viewListSorterState : SortableColumn -> State -> State
viewListSorterState column (State direction _) =
    if direction == Asc then
        State Desc column
    else
        State Asc column


combineUploadsWithFiles : List file -> UploadId.Collection (Upload.UploadingFile file) -> List (UploadState file)
combineUploadsWithFiles files uploads =
    List.concat
        [ List.map (uncurry Uploading) <| UploadId.toList uploads
        , List.map Uploaded files
        ]


viewRow : Config file msg -> UploadState file -> Html msg
viewRow config file =
    case file of
        Uploading uploadId uploadingFile ->
            viewUploadingRow config uploadId uploadingFile

        Uploaded file ->
            viewUploadedRow config file


viewUploadingRow : Config file msg -> UploadId -> UploadingFile file -> Html msg
viewUploadingRow (Config { cancelUploadMsg, columns }) uploadId file =
    tr []
        [ td [] []
        , td [] [ viewUploadingThumbnail file ]
        , td [] [ text <| Upload.fileName file ]
        , td
            [ colspan (List.length columns) ]
            [ progress
                [ Attributes.max "100.0"
                , Attributes.value (Upload.uploadPercentage file |> toString)
                ]
                []
            ]
        , td [] [ button [ onClick (cancelUploadMsg uploadId) ] [ text "Cancel" ] ]
        ]


viewUploadingThumbnail : UploadingFile file -> Html msg
viewUploadingThumbnail file =
    img
        [ style thumbnailStyle
        , src
            (file
                |> Upload.base64EncodedData
                |> Maybe.map Base64Encoded.toString
                |> Maybe.withDefault ""
            )
        ]
        []


viewUploadedRow : Config file msg -> file -> Html msg
viewUploadedRow ((Config { nameFn, thumbnailSrcFn, columns }) as config) file =
    tr []
        (List.concat
            [ [ td [] [ input [ type_ "checkbox" ] [] ]
              , td [] [ viewUploadedThumbnail config file ]
              , td [] [ text (nameFn file) ]
              ]
            , viewUserDefinedTds file columns
            , [ td [] [] ]
            ]
        )


viewUserDefinedTds : file -> List (Column file msg) -> List (Html msg)
viewUserDefinedTds file =
    List.map
        (\{ html } ->
            td [] [ html file ]
        )


viewUploadedThumbnail : Config file msg -> file -> Html msg
viewUploadedThumbnail (Config { thumbnailSrcFn }) file =
    img
        [ style thumbnailStyle
        , src (thumbnailSrcFn file)
        ]
        []


thumbnailStyle : List ( String, String )
thumbnailStyle =
    [ ( "object-fit", "cover" )
    , ( "width", "50px" )
    , ( "height", "50px" )
    ]
