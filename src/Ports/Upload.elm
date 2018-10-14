port module Ports.Upload exposing (fileContentRead, fileContentReadFailed, readFileContent, uploadCancelled, uploadFailed, uploadPort, uploadProgress, uploaded)

import Json.Decode as Decode
import Json.Encode as Encode


{-| A port used to update the progress of the upload from JS-land; Encode.Value is the UploadId and Float is the percent
-}
port uploadProgress : (( Encode.Value, Float ) -> msg) -> Sub msg


{-| A port used to cancel the upload in JS-land. Encode.Value is the UploadId
-}
port uploadCancelled : Encode.Value -> Cmd msg


{-| A port used to tell JS-land to read the Base64 file content of a file.
The Encode.Value is the UploadId
The Decode.Value is the raw JS file event. For more details see the Drag.File documentation.
-}
port readFileContent : ( Encode.Value, Decode.Value ) -> Cmd msg


port fileContentReadFailed : (Encode.Value -> msg) -> Sub msg


{-| A port used to update the internal file with the Base64 encoded file
-}
port fileContentRead : (Encode.Value -> msg) -> Sub msg


{-| A port used to tell JS-land to actually upload a file to S3. Sends the UploadId, SignedUrl and Base64Encoded data
The encode values are:fileContentRead

  - UploadId
  - SignedUrl
  - Base64Encoded

-}
port uploadPort : { uploadId : Encode.Value, uploadUrl : Encode.Value, base64Data : Encode.Value, additionalData : Encode.Value } -> Cmd msg


{-| A port used to tell the internal state that an upload has failed, and to update accordingly}
-}
port uploadFailed : (Encode.Value -> msg) -> Sub msg


{-| A port used to tell the internal state that the file has been successfully uploaded
-}
port uploaded : (( Encode.Value, Encode.Value ) -> msg) -> Sub msg
