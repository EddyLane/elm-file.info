module File.UploadId
    exposing
        ( Collection
        , UploadId
        , collection
        , decoder
        , encoder
        , get
        , incrementIdBy
        , init
        , insert
        , remove
        , toList
        , update
        )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


---- TYPES ----


type UploadId
    = UploadId Int


type Collection file
    = Collection (Dict Int file)


init : UploadId
init =
    UploadId 1



---- COLLECTION ----


collection : Collection file
collection =
    Collection Dict.empty


values : Collection file -> List file
values (Collection collection) =
    Dict.values collection


get : UploadId -> Collection file -> Maybe file
get (UploadId id) (Collection collection) =
    Dict.get id collection


insert : UploadId -> file -> Collection file -> Collection file
insert (UploadId id) file (Collection collection) =
    Dict.insert id file collection
        |> Collection


remove : UploadId -> Collection file -> Collection file
remove (UploadId id) (Collection collection) =
    Dict.remove id collection
        |> Collection


toList : Collection file -> List ( UploadId, file )
toList (Collection collection) =
    collection
        |> Dict.toList
        |> List.map (Tuple.mapFirst UploadId)


update : UploadId -> (Maybe file -> Maybe file) -> Collection file -> Collection file
update (UploadId id) updateFn (Collection collection) =
    Dict.update id updateFn collection
        |> Collection



---- UPDATE -----


incrementIdBy : Int -> UploadId -> UploadId
incrementIdBy i (UploadId id) =
    UploadId (i + id)



---- CREATE ----


decoder : Decoder UploadId
decoder =
    Decode.map UploadId Decode.int


encoder : UploadId -> Encode.Value
encoder (UploadId id) =
    Encode.int id
