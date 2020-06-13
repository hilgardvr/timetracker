module View.Styles exposing (..)

import Element exposing (..)
import Element.Background as Background
import Model.Model exposing (Model)

primaryColor: Color
primaryColor = rgb255 157 255 209

lightColor: Color
lightColor = rgb255 209 255 255

darkColor: Color
darkColor = rgb255 106 203 160

focussedColor: Color
focussedColor = rgb255 111 111 111

debugColor: Color
debugColor = rgb255 255 0 0

maxProjectShownSize: Int
maxProjectShownSize = 16

navBarHeight: Int
navBarHeight = 50

edges: { bottom: number, left: number, right: number, top: number }
edges = 
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }

cardWidth: Model -> Int
cardWidth model =
    if model.window.width > 500
    then 500
    else model.window.width - 50

loginCardWidth: Model -> Int
loginCardWidth model =
    300

cardAttributes: Model -> List (Attribute msg)
cardAttributes model =
    [ width <| px <| cardWidth model, Background.color lightColor, centerX ]

loginCardAttributes: Model -> List (Attribute msg)
loginCardAttributes model =
    [ width <| px <| loginCardWidth model, Background.color lightColor, centerX ]