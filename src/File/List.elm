module File.List
    exposing
        ( Config
        , Sort(..)
        , State
        , cancelUploadMsg
        , column
        , columns
        , config
        , contentTypeFn
        , defaultSort
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


type Config columnId file msg
    = Config (ConfigRec columnId file msg)


type alias ConfigRec columnId file msg =
    { nameFn : file -> String
    , contentTypeFn : file -> String
    , thumbnailSrcFn : file -> String
    , cancelUploadMsg : UploadId -> msg
    , columns : List (Column columnId file msg)
    , setListStateMsg : State columnId -> msg
    , defaultCustomSort : Maybe (Sort columnId)
    }


type State id
    = State SortDirection (Sort id)


type alias StateRec =
    { direction : SortDirection
    , column : Sort
    }


type SortDirection
    = Asc
    | Desc


type Sort id
    = SortByFilename
    | SortByCustom id


type alias Column id file msg =
    { id : id
    , label : String
    , html : file -> Html msg
    , sorter : file -> file -> Order
    }


init : Config columnId file msg -> State columnId
init (Config { defaultCustomSort }) =
    case defaultCustomSort of
        Just column ->
            State Desc column

        Nothing ->
            State Desc SortByFilename


defaultSort : Sort columnId -> Config columnId file msg -> Config columnId file msg
defaultSort sort (Config configRec) =
    Config <|
        { configRec | defaultCustomSort = Just sort }



---- CONFIG ----


config : msg -> Config columnId file msg
config noOpMsg =
    Config <|
        { nameFn = always "-"
        , contentTypeFn = always "-"
        , cancelUploadMsg = always noOpMsg
        , thumbnailSrcFn = always ""
        , columns = []
        , setListStateMsg = always noOpMsg
        , defaultCustomSort = Nothing
        }


setListStateMsg : (State columnId -> msg) -> Config columnId file msg -> Config columnId file msg
setListStateMsg msg (Config configRec) =
    Config <|
        { configRec | setListStateMsg = msg }


nameFn : (file -> String) -> Config columnId file msg -> Config columnId file msg
nameFn fn (Config configRec) =
    Config <|
        { configRec | nameFn = fn }


contentTypeFn : (file -> String) -> Config columnId file msg -> Config columnId file msg
contentTypeFn fn (Config configRec) =
    Config <|
        { configRec | contentTypeFn = fn }


thumbnailSrcFn : (file -> String) -> Config columnId file msg -> Config columnId file msg
thumbnailSrcFn fn (Config configRec) =
    Config <|
        { configRec | thumbnailSrcFn = fn }


cancelUploadMsg : (UploadId -> msg) -> Config columnId file msg -> Config columnId file msg
cancelUploadMsg msg (Config configRec) =
    Config <|
        { configRec | cancelUploadMsg = msg }


columns : List (Column columnId file msg) -> Config columnId file msg -> Config columnId file msg
columns columns (Config configRec) =
    Config <|
        { configRec | columns = columns }


column : Column columnId file msg -> Config columnId file msg -> Config columnId file msg
column col (Config configRec) =
    Config <|
        { configRec | columns = col :: configRec.columns }



---- VIEW ----


view : State columnId -> Config columnId file msg -> Upload.State file -> List file -> Html msg
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


sortResults : Config columnId file msg -> State columnId -> List (UploadState file) -> List (UploadState file)
sortResults config (State sortDir sortCol) =
    case sortCol of
        SortByFilename ->
            List.sortWith (sortByFilename config sortDir)

        SortByCustom headerName ->
            List.sortWith (sortByCustom headerName config sortDir)


sortByCustom : columnId -> Config columnId file msg -> SortDirection -> UploadState file -> UploadState file -> Order
sortByCustom columnId ((Config { columns }) as config) sortDir a b =
    columns
        |> List.filter (\{ id } -> id == columnId)
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


sortByFilename : Config columnId file msg -> SortDirection -> UploadState file -> UploadState file -> Order
sortByFilename config sortDir a b =
    case sortDirPair sortDir a b of
        ( a, b ) ->
            Basics.compare (filename config a) (filename config b)


filename : Config columnId file msg -> UploadState file -> String
filename (Config { nameFn }) file =
    case file of
        Uploaded file ->
            nameFn file

        Uploading _ file ->
            Upload.fileName file


sortDirPair : SortDirection -> a -> a -> ( a, a )
sortDirPair sortDir a b =
    if sortDir == Asc then
        ( a, b )
    else
        ( b, a )


viewTableHeader : State columnId -> Config columnId file msg -> Html msg
viewTableHeader ((State direction column) as state) ((Config { columns, setListStateMsg }) as config) =
    thead []
        [ tr []
            (List.concat
                [ [ th [] [ text "" ]
                  , th [] [ text "" ]
                  , th
                        [ style [ ( "cursor", "pointer" ) ]
                        , onClick (setListStateMsg (viewListSorterState SortByFilename state))
                        ]
                        [ text "File"
                        , if column == SortByFilename then
                            viewActiveSortArrow direction
                          else
                            text ""
                        ]
                  ]
                , viewUserDefinedThs state config
                , [ th [] [ text "" ] ]
                ]
            )
        ]


viewCustomSortArrow : columnId -> State columnId -> Html msg
viewCustomSortArrow columnId (State sortDir sortCol) =
    case sortCol of
        SortByCustom id ->
            if id == columnId then
                viewActiveSortArrow sortDir
            else
                text ""

        SortByFilename ->
            text ""


viewActiveSortArrow : SortDirection -> Html msg
viewActiveSortArrow dir =
    if dir == Asc then
        text "↑"
    else
        text "↓"


viewUserDefinedThs : State columnId -> Config columnId file msg -> List (Html msg)
viewUserDefinedThs state (Config { columns, setListStateMsg }) =
    List.map
        (\{ label, id } ->
            th
                [ style [ ( "cursor", "pointer" ) ]
                , onClick <|
                    setListStateMsg <|
                        viewListSorterState (SortByCustom id) state
                ]
                [ text label
                , viewCustomSortArrow id state
                ]
        )
        columns


viewListSorterState : Sort columnId -> State columnId -> State columnId
viewListSorterState column (State direction col) =
    if direction == Asc && column == col then
        State Desc column
    else
        State Asc column


combineUploadsWithFiles : List file -> UploadId.Collection (Upload.UploadingFile file) -> List (UploadState file)
combineUploadsWithFiles files uploads =
    List.concat
        [ List.map (uncurry Uploading) <| UploadId.toList uploads
        , List.map Uploaded files
        ]


viewRow : Config columnId file msg -> UploadState file -> Html msg
viewRow config file =
    case file of
        Uploading uploadId uploadingFile ->
            viewUploadingRow config uploadId uploadingFile

        Uploaded file ->
            viewUploadedRow config file


viewUploadingRow : Config columnId file msg -> UploadId -> UploadingFile file -> Html msg
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


viewUploadedRow : Config columnId file msg -> file -> Html msg
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


viewUserDefinedTds : file -> List (Column id file msg) -> List (Html msg)
viewUserDefinedTds file =
    List.map
        (\{ html } ->
            td [] [ html file ]
        )


viewUploadedThumbnail : Config columnId file msg -> file -> Html msg
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
