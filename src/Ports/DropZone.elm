port module Ports.DropZone exposing (..)

{-| A port used to trigger the onClick event on an actual file input, specified by the String param
-}


port openFileBrowser : String -> Cmd msg
