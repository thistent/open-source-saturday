module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



-- Model & Types --


type alias Model =
    String



-- Message Type --


type Msg
    = NoMsg



-- Initial Model --


init : () -> ( Model, Cmd Msg )
init _ =
    ( "hi", Cmd.none )



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
    { title = ""
    , body =
        [ Element.layout
            [ padding 20 ]
          <|
            text model
        ]
    }



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    Sub.batch []
