module View.Styles exposing (..)

import Model.Model as M
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Element as E
import Update.Update exposing (initViewport)
import Model.Model exposing (Msg)

primaryColor: E.Color
primaryColor = E.rgb255 157 255 209

lightColor: E.Color
lightColor = E.rgb255 209 255 255

darkColor: E.Color
darkColor = E.rgb255 106 203 160

focussedColor: E.Color
focussedColor = E.rgb255 111 111 111

debugColor: E.Color
debugColor = E.rgb255 255 0 0

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

cardWidth: M.Model -> Int
cardWidth model =
    if model.window.width > 500
    then 500
    else model.window.width - 50

loginCardWidth: M.Model -> Int
loginCardWidth model =
    300

cardAttributes: M.Model -> List (E.Attribute msg)
cardAttributes model =
    [ E.width <| E.px <| cardWidth model, Background.color primaryColor, E.centerX ]

loginCardAttributes: M.Model -> List (E.Attribute msg)
loginCardAttributes model =
    [ E.width <| E.px <| loginCardWidth model, Background.color primaryColor, E.centerX ]

buttonAttributes: List (E.Attribute msg)
buttonAttributes =
    [ Background.color darkColor
    , Border.glow darkColor 1
    , E.focused [ Background.color focussedColor ]
    , Border.rounded 5
    , Font.bold
    , E.width <| E.px 80
    ]

getWidth: M.Model -> E.Attribute msg
getWidth model =
    if model.window.width > 360
    then E.width <| E.px 300
    else E.width E.fill

getTimeframeWidth: M.Model -> E.Attribute msg
getTimeframeWidth model =
    E.width <| E.px 100