module File.List
    exposing
        ( Config
        , Sort(..)
        , SortDirection(..)
        , State
        , cancelUploadMsg
        , column
        , columns
        , config
        , contentTypeFn
        , defaultSort
        , defaultSortDirection
        , idFn
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
import Set exposing (Set)


type UploadState file
    = Uploading UploadId (UploadingFile file)
    | Uploaded file


type Config colId file msg
    = Config (ConfigRec colId file msg)


type alias ConfigRec colId file msg =
    { idFn : file -> String
    , nameFn : file -> String
    , contentTypeFn : file -> String
    , thumbnailSrcFn : file -> String
    , cancelUploadMsg : UploadId -> msg
    , columns : List (Column colId file msg)
    , setListStateMsg : State colId -> msg
    , defaultCustomSort : Maybe (Sort colId)
    , defaultSortDir : SortDirection
    }


type State id
    = State (StateRec id)


type alias StateRec id =
    { direction : SortDirection
    , sortColumn : Sort id
    , selectedIds : Set String
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


init : Config colId file msg -> State colId
init (Config { defaultCustomSort, defaultSortDir }) =
    State <|
        { direction = defaultSortDir
        , sortColumn = Maybe.withDefault SortByFilename defaultCustomSort
        , selectedIds = Set.empty
        }


defaultSort : Sort colId -> Config colId file msg -> Config colId file msg
defaultSort sort (Config configRec) =
    Config <|
        { configRec | defaultCustomSort = Just sort }


defaultSortDirection : SortDirection -> Config colId file msg -> Config colId file msg
defaultSortDirection sortDir (Config configRec) =
    Config <|
        { configRec | defaultSortDir = sortDir }



---- CONFIG ----


config : msg -> Config colId file msg
config noOpMsg =
    Config <|
        { idFn = always "-"
        , nameFn = always "-"
        , contentTypeFn = always "-"
        , cancelUploadMsg = always noOpMsg
        , thumbnailSrcFn = always ""
        , columns = []
        , setListStateMsg = always noOpMsg
        , defaultCustomSort = Nothing
        , defaultSortDir = Asc
        }


setListStateMsg : (State colId -> msg) -> Config colId file msg -> Config colId file msg
setListStateMsg msg (Config configRec) =
    Config <|
        { configRec | setListStateMsg = msg }


defaultDirection : (State colId -> msg) -> Config colId file msg -> Config colId file msg
defaultDirection msg (Config configRec) =
    Config <|
        { configRec | setListStateMsg = msg }


idFn : (file -> String) -> Config colId file msg -> Config colId file msg
idFn fn (Config configRec) =
    Config <|
        { configRec | idFn = fn }


nameFn : (file -> String) -> Config colId file msg -> Config colId file msg
nameFn fn (Config configRec) =
    Config <|
        { configRec | nameFn = fn }


contentTypeFn : (file -> String) -> Config colId file msg -> Config colId file msg
contentTypeFn fn (Config configRec) =
    Config <|
        { configRec | contentTypeFn = fn }


thumbnailSrcFn : (file -> String) -> Config colId file msg -> Config colId file msg
thumbnailSrcFn fn (Config configRec) =
    Config <|
        { configRec | thumbnailSrcFn = fn }


cancelUploadMsg : (UploadId -> msg) -> Config colId file msg -> Config colId file msg
cancelUploadMsg msg (Config configRec) =
    Config <|
        { configRec | cancelUploadMsg = msg }


columns : List (Column colId file msg) -> Config colId file msg -> Config colId file msg
columns columns (Config configRec) =
    Config <|
        { configRec | columns = columns }


column : Column colId file msg -> Config colId file msg -> Config colId file msg
column col (Config configRec) =
    Config <|
        { configRec | columns = col :: configRec.columns }



---- VIEW ----


view : State colId -> Config colId file msg -> Upload.State file -> List file -> Html msg
view ((State { direction, sortColumn }) as state) config upload files =
    table []
        [ viewTableHeader state config
        , tbody []
            (upload
                |> Upload.uploads
                |> combineUploadsWithFiles files
                |> sortResults config state
                |> List.map (viewRow config state)
            )
        ]


sortResults : Config colId file msg -> State colId -> List (UploadState file) -> List (UploadState file)
sortResults config (State { direction, sortColumn }) =
    case sortColumn of
        SortByFilename ->
            List.sortWith (sortByFilename config direction)

        SortByCustom headerName ->
            List.sortWith (sortByCustom headerName config direction)


sortByCustom : colId -> Config colId file msg -> SortDirection -> UploadState file -> UploadState file -> Order
sortByCustom colId ((Config { columns }) as config) sortDir a b =
    columns
        |> List.filter (\{ id } -> id == colId)
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


sortByFilename : Config colId file msg -> SortDirection -> UploadState file -> UploadState file -> Order
sortByFilename config sortDir a b =
    case sortDirPair sortDir a b of
        ( a, b ) ->
            Basics.compare (filename config a) (filename config b)


filename : Config colId file msg -> UploadState file -> String
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


viewTableHeader : State colId -> Config colId file msg -> Html msg
viewTableHeader ((State { direction, sortColumn }) as state) ((Config { columns, setListStateMsg }) as config) =
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
                        , if sortColumn == SortByFilename then
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


viewCustomSortArrow : colId -> State colId -> Html msg
viewCustomSortArrow colId (State { direction, sortColumn }) =
    case sortColumn of
        SortByCustom id ->
            if id == colId then
                viewActiveSortArrow direction
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


viewUserDefinedThs : State colId -> Config colId file msg -> List (Html msg)
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


viewListSorterState : Sort colId -> State colId -> State colId
viewListSorterState column (State state) =
    if state.direction == Asc && state.sortColumn == column then
        State <|
            { state
                | direction = Desc
                , sortColumn = state.sortColumn
            }
    else
        State <|
            { state
                | direction = Asc
                , sortColumn = state.sortColumn
            }


combineUploadsWithFiles : List file -> UploadId.Collection (Upload.UploadingFile file) -> List (UploadState file)
combineUploadsWithFiles files uploads =
    List.concat
        [ List.map (uncurry Uploading) <| UploadId.toList uploads
        , List.map Uploaded files
        ]


viewRow : Config colId file msg -> State colId -> UploadState file -> Html msg
viewRow ((Config { idFn }) as config) state file =
    case file of
        Uploading uploadId uploadingFile ->
            viewUploadingRow config uploadId uploadingFile

        Uploaded file ->
            viewUploadedRow config state file


viewUploadingRow : Config colId file msg -> UploadId -> UploadingFile file -> Html msg
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


viewUploadedRow : Config colId file msg -> State colId -> file -> Html msg
viewUploadedRow ((Config configRec) as config) ((State { selectedIds }) as state) file =
    tr []
        (List.concat
            [ [ td []
                    [ input
                        [ type_ "checkbox"
                        , checked (Set.member (configRec.idFn file) selectedIds)
                        , onClick (configRec.setListStateMsg <| toggleFile config file state)
                        ]
                        []
                    ]
              , td [] [ viewUploadedThumbnail config file ]
              , td [] [ text (configRec.nameFn file) ]
              ]
            , viewUserDefinedTds file configRec.columns
            , [ td [] [] ]
            ]
        )


toggleFile : Config colId file msg -> file -> State colId -> State colId
toggleFile (Config { idFn }) file (State state) =
    let
        id =
            idFn file
    in
    State <|
        { state
            | selectedIds =
                if Set.member id state.selectedIds then
                    Set.remove id state.selectedIds
                else
                    Set.insert id state.selectedIds
        }


viewUserDefinedTds : file -> List (Column id file msg) -> List (Html msg)
viewUserDefinedTds file =
    List.map
        (\{ html } ->
            td [] [ html file ]
        )


viewUploadedThumbnail : Config colId file msg -> file -> Html msg
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
