module File.UploadId
    exposing
        ( Collection
        , UploadId
        , decoder
        , encoder
        , get
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
    = Collection UploadId (Dict Int file)


init : Collection file
init =
    Collection (UploadId 1) Dict.empty


values : Collection file -> List file
values (Collection _ collection) =
    Dict.values collection


get : UploadId -> Collection file -> Maybe file
get (UploadId id) (Collection _ collection) =
    Dict.get id collection


insert : file -> Collection file -> ( UploadId, Collection file )
insert file (Collection (UploadId id) collection) =
    ( UploadId (id + 1)
    , Dict.insert id file collection
        |> Collection (UploadId (id + 1))
    )


remove : UploadId -> Collection file -> Collection file
remove (UploadId id) (Collection uploadId collection) =
    Dict.remove id collection
        |> Collection uploadId


toList : Collection file -> List ( UploadId, file )
toList (Collection _ collection) =
    collection
        |> Dict.toList
        |> List.map (Tuple.mapFirst UploadId)


update : UploadId -> (Maybe file -> Maybe file) -> Collection file -> Collection file
update (UploadId id) updateFn (Collection uploadId collection) =
    Dict.update id updateFn collection
        |> Collection uploadId



---- CREATE ----


decoder : Decoder UploadId
decoder =
    Decode.map UploadId Decode.int


encoder : UploadId -> Encode.Value
encoder (UploadId id) =
    Encode.int id
