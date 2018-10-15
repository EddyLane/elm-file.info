module View.Helpers exposing (icon)

import Element exposing (..)
import FeatherIcons


icon : FeatherIcons.Icon -> Element msg
icon featherIcon =
    featherIcon
        |> FeatherIcons.toHtml []
        |> html
