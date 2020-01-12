module Main exposing (..)

import Browser
import Color
import Color.Manipulate exposing (darken, lighten)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



-- Model & Types --


type alias Model =
    { page : Page
    , meetups : List Meetup
    , meetupForm : Maybe Meetup
    }


type Page
    = AboutPage
    | ProjectsPage


type alias Meetup =
    { address : Address
    , venueName : String
    , image : String
    , dateTime : String
    }


type alias Address =
    { street : String
    , city : String
    , state : String
    , zip : String
    }



-- Message Type --


type Msg
    = ChangePage Page
    | AddMeetup Meetup
    | OpenMeetupForm
    | CloseMeetupForm
    | UpdateForm MeetupMsg


type MeetupMsg
    = Venue String
    | Street String
    | City String
    | State String
    | Zip String
    | Image String
    | DateTime String



-- Initial Model --


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = AboutPage
      , meetups =
            [ { address =
                    { street = "2780 La Mirada Dr. Suite E"
                    , city = "Vista"
                    , state = "CA"
                    , zip = "92081"
                    }
              , venueName = "Open Source Maker Labs"
              , image = ":)"
              , dateTime = "now"
              }
            , { address =
                    { street = "2780 La Mirada Dr. Suite E"
                    , city = "Vista"
                    , state = "CA"
                    , zip = "92081"
                    }
              , venueName = "Open Source Maker Labs"
              , image = ":)"
              , dateTime = "now"
              }
            , { address =
                    { street = "2780 La Mirada Dr. Suite E"
                    , city = "Vista"
                    , state = "CA"
                    , zip = "92081"
                    }
              , venueName = "Open Source Maker Labs"
              , image = ":)"
              , dateTime = "now"
              }
            ]
      , meetupForm = Nothing
      }
    , Cmd.none
    )


emptyMeetup : Meetup
emptyMeetup =
    { address =
        { street = ""
        , city = ""
        , state = ""
        , zip = ""
        }
    , venueName = ""
    , image = ""
    , dateTime = ""
    }



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
                , column
                    [ height fill
                    , width fill
                    , padding 20
                    ]
                    [ el [ Font.size 32, padding 15, centerX ] <| text "Meetups"
                    , column
                        [ centerX
                        , Border.width 2
                        , Border.color <| rgb 0.7 0.7 0.7
                        ]
                      <|
                        List.indexedMap meetupToEl model.meetups
                    , column
                        [ centerX, padding 10 ]
                        [ case model.meetupForm of
                            Just meetup ->
                                column [ width fill ]
                                    [ Input.text []
                                        { onChange = \x -> UpdateForm <| Venue x
                                        , text = .venueName <| Maybe.withDefault emptyMeetup model.meetupForm
                                        , placeholder = Nothing
                                        , label =
                                            Input.labelAbove [] <|
                                                text "Venue Name"
                                        }
                                    , Input.button []
                                        { onPress = Just CloseMeetupForm
                                        , label =
                                            el
                                                [ Bg.color primaryColor
                                                , Font.color textColor
                                                , paddingXY 20 10
                                                , Border.rounded 5
                                                ]
                                            <|
                                                text
                                                    "Close Form"
                                        }
                                    ]

                            Nothing ->
                                Input.button []
                                    { onPress = Just OpenMeetupForm
                                    , label =
                                        el
                                            [ Bg.color primaryColor
                                            , Font.color textColor
                                            , paddingXY 20 10
                                            , Border.rounded 5
                                            ]
                                        <|
                                            text
                                                "Add Meetup"
                                    }
                        ]
                    ]
                , el
                    [ padding 15
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

        AddMeetup meetup ->
            ( { model | meetups = model.meetups ++ [ meetup ] }, Cmd.none )

        OpenMeetupForm ->
            ( { model | meetupForm = Just emptyMeetup }, Cmd.none )

        CloseMeetupForm ->
            ( { model | meetupForm = Nothing }, Cmd.none )

        UpdateForm meetupMsg ->
            case model.meetupForm of
                Nothing ->
                    ( model, Cmd.none )

                Just currentMeetup ->
                    (case meetupMsg of
                        Venue venueName ->
                            let
                                newMeetupForm =
                                    { currentMeetup | venueName = venueName }
                            in
                            ( { model
                                | meetupForm = Just newMeetupForm
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )
                     {- Street street ->
                        City cityName ->
                     -}
                    )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    Sub.batch []



-- Helper Functions --


mapColor : (Color.Color -> Color.Color) -> Color -> Color
mapColor fun =
    fromElColor >> fun >> toElColor


fromElColor : Color -> Color.Color
fromElColor =
    toRgb >> Color.fromRgba


toElColor : Color.Color -> Color
toElColor =
    Color.toRgba >> fromRgb


meetupToEl : Int -> Meetup -> Element Msg
meetupToEl num meetup =
    row
        [ padding 20
        , spacing 40
        , Bg.color <|
            case modBy 2 num of
                0 ->
                    rgb 0.8 0.8 0.8

                _ ->
                    textColor
        ]
        [ el [ width fill, Font.bold ] <|
            text <|
                String.fromInt (num + 1)
                    ++ "."
        , el [ width fill, Font.bold ] <| text meetup.venueName
        , el [ width fill ] <| text <| meetup.address.city ++ ", " ++ meetup.address.state
        , el [ width fill ] <| text meetup.dateTime
        ]



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
