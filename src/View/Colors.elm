module View.Colors exposing (..)

import Element as Element

primaryColor: Element.Color
primaryColor = Element.rgb255 157 255 209

lightColor: Element.Color
lightColor = Element.rgb255 209 255 255

darkColor: Element.Color
darkColor = Element.rgb255 106 203 160

focussedColor: Element.Color
focussedColor = Element.rgb255 111 111 111

debugColor: Element.Color
debugColor = Element.rgb255 255 0 0

maxProjectShownSize: Int
maxProjectShownSize = 16

edges: { bottom: number, left: number, right: number, top: number }
edges = 
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }