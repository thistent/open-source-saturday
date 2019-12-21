module Main exposing (..)

import Browser
import Color
import Color.Manipulate exposing (darken, lighten)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



-- Color Palette --


primaryColor : Color
primaryColor =
    rgb 0.67 0.09 0.09


textColor : Color
textColor =
    rgb 0.9 0.9 0.9


primaryColorDark : Color
primaryColorDark =
    primaryColor |> mapColor (darken 0.25)


mapColor : (Color.Color -> Color.Color) -> Color -> Color
mapColor fun =
    fromElColor >> fun >> toElColor



-- Model & Types --


type alias Model =
    { page : Page
    }


type Page
    = AboutPage
    | ProjectsPage



-- Message Type --


type Msg
    = ChangePage Page



-- Initial Model --


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = AboutPage }, Cmd.none )



-- Main App --


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }



-- View --


view : Model -> Browser.Document Msg
view model =
    { title = "Open Source Saturday"
    , body =
        [ Element.layout
            [ Font.color primaryColor
            , Bg.color textColor
            ]
          <|
            column [ height fill, width fill, spacing 5 ]
                [ el
                    [ padding 20
                    , width fill
                    , Font.size 30
                    , Font.color textColor
                    , Bg.color primaryColor
                    , Border.shadow
                        { offset = ( 0, 0 )
                        , size = 3
                        , blur = 3
                        , color = rgba 0 0 0 0.25
                        }
                    ]
                  <|
                    text "Open Source Saturday"
                , el
                    [ height fill
                    , width fill
                    , padding 20
                    ]
                  <|
                    text "Hello World!"
                , el
                    [ padding 20
                    , width fill
                    , Font.color textColor
                    , Font.alignRight
                    , Bg.color primaryColorDark
                    ]
                  <|
                    text "© 2020"
                ]
        ]
    }



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            ( { model | page = page }, Cmd.none )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    Sub.batch []



-- Helper Functions --


fromElColor : Color -> Color.Color
fromElColor =
    toRgb >> Color.fromRgba


toElColor : Color.Color -> Color
toElColor =
    Color.toRgba >> fromRgb
