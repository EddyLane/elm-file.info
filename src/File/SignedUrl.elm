module File.SignedUrl exposing (SignedUrl, decoder, toString)

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


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


toString : SignedUrl -> String
toString (SignedUrl { signedUrl }) =
    signedUrl
