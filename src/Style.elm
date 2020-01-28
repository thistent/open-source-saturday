module Style exposing (..)

--import Element.Events as Event
--import Element.Font as Font
--import Element.Input as Input

import Color
import Color.Manipulate exposing (darken, lighten)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Html.Attributes exposing (style)
import Model



-- Attribute Lists --


disableScroll : List (Attribute Model.Msg)
disableScroll =
    [ htmlAttribute <| style "max-height" "100vh"
    , htmlAttribute <| style "overflow-y" "hidden"
    ]


corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }


roundBottomCorners =
    Border.roundEach
        { corners | bottomLeft = 10, bottomRight = 10 }


roundTopCorners =
    Border.roundEach
        { corners | topLeft = 10, topRight = 10 }


shadow : Attribute Model.Msg
shadow =
    Border.shadow
        { offset = ( 0, 3 )
        , size = 1
        , blur = 4
        , color = rgba 0 0 0 0.25
        }



-- Color Palette --


primaryColor : Color
primaryColor =
    rgb 0.67 0.09 0.09


textColor : Color
textColor =
    rgb 0.9 0.9 0.9


textColorMid : Color
textColorMid =
    textColor |> mapColor (darken 0.15)


textColorDark : Color
textColorDark =
    textColor |> mapColor (darken 0.25)


textColorExtraDark : Color
textColorExtraDark =
    textColor |> mapColor (darken 0.65)


primaryColorDark : Color
primaryColorDark =
    primaryColor |> mapColor (darken 0.25)



-- Color Functions --


mapColor : (Color.Color -> Color.Color) -> Color -> Color
mapColor fun =
    fromElColor >> fun >> toElColor


fromElColor : Color -> Color.Color
fromElColor =
    toRgb >> Color.fromRgba


toElColor : Color.Color -> Color
toElColor =
    Color.toRgba >> fromRgb


addAlpha : Float -> Color -> Color
addAlpha alpha color =
    let
        colorRec =
            toRgb color
    in
    fromRgb { colorRec | alpha = alpha }


alternateColors : Int -> Attribute msg
alternateColors index =
    Bg.color <|
        case modBy 2 index of
            0 ->
                addAlpha 0.5 textColor

            _ ->
                addAlpha 0.5 textColorMid
