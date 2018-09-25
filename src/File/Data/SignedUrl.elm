module File.Data.SignedUrl exposing (SignedUrl, decoder, encoder)

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


type SignedUrl
    = SignedUrl SignedUrlRec


type alias SignedUrlRec =
    { signedUrl : String
    , reference : String
    }


decoder : Decode.Decoder SignedUrl
decoder =
    Pipeline.decode SignedUrlRec
        |> Pipeline.required "signedUrl" Decode.string
        |> Pipeline.required "reference" Decode.string
        |> Decode.andThen (SignedUrl >> Decode.succeed)


encoder : SignedUrl -> Encode.Value
encoder (SignedUrl { signedUrl }) =
    Encode.string signedUrl
