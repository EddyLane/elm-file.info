module File.Base64Encoded exposing (Base64Encoded, decoder, encoder, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Base64Encoded
    = Base64Encoded String


toString : Base64Encoded -> String
toString (Base64Encoded base64) =
    base64


decoder : Decoder Base64Encoded
decoder =
    Decode.string
        |> Decode.andThen (Base64Encoded >> Decode.succeed)


encoder : Base64Encoded -> Encode.Value
encoder (Base64Encoded base64) =
    Encode.string base64
