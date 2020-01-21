module Main exposing (..)

import Array exposing (Array)
import Browser
import Color
import Color.Manipulate exposing (darken, lighten)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html.Attributes exposing (style)



-- Model & Types --


type alias Model =
    { page : Page
    , meetups : Array ( Meetup, Bool )
    , meetupForm : Maybe Meetup
    }


type Page
    = AboutPage
    | ProjectsPage


type alias Meetup =
    { venueName : String
    , image : String
    , dateTime : String
    , street1 : String
    , street2 : String
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
    | ToggleExpanded Int


type MeetupMsg
    = Venue String
    | Street1 String
    | Street2 String
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
            Array.fromList
                [ ( { street1 = "2780 La Mirada Dr."
                    , street2 = "Suite E"
                    , city = "Vista"
                    , state = "CA"
                    , zip = "92081"
                    , venueName = "Open Source Maker Labs"
                    , image = ":)"
                    , dateTime = "now"
                    }
                  , False
                  )
                , ( { street1 = "2780 La Mirada Dr."
                    , street2 = "Suite E"
                    , city = "Vista"
                    , state = "CA"
                    , zip = "92081"
                    , venueName = "Open Source Maker Labs"
                    , image = ":)"
                    , dateTime = "now"
                    }
                  , False
                  )
                , ( { street1 = "2780 La Mirada Dr."
                    , street2 = "Suite E"
                    , city = "Vista"
                    , state = "CA"
                    , zip = "92081"
                    , venueName = "Open Source Maker Labs"
                    , image = ":)"
                    , dateTime = "now"
                    }
                  , False
                  )
                ]
      , meetupForm = Nothing
      }
    , Cmd.none
    )


emptyMeetup : Meetup
emptyMeetup =
    { street1 = ""
    , street2 = ""
    , city = ""
    , state = ""
    , zip = ""
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
            ([ Font.color primaryColor
             , Bg.color textColorDark

             --, clip
             , behindContent <|
                image
                    [ centerX
                    , width fill
                    , alpha 0.75
                    , Border.shadow
                        { offset = ( 0, 3 )
                        , size = 1
                        , blur = 4
                        , color = rgba 0 0 0 0.25
                        }
                    ]
                    { src = "/static/open-source-saturday.jpg"
                    , description = "People at open source saturday!"
                    }
             , inFront <|
                el
                    [ padding 20
                    , width fill
                    , Font.size 30
                    , Font.color textColor
                    , Bg.color primaryColor
                    , alpha 0.8
                    , Border.roundEach
                        { topLeft = 0
                        , topRight = 0
                        , bottomLeft = 10
                        , bottomRight = 10
                        }
                    , Border.shadow
                        { offset = ( 0, 3 )
                        , size = 1
                        , blur = 4
                        , color = rgba 0 0 0 0.25
                        }
                    ]
                <|
                    text "Open Source Saturday"
             , inFront <|
                case model.meetupForm of
                    Just _ ->
                        el
                            [ width fill
                            , height fill
                            , Bg.color <| rgba 0 0 0 0.75
                            ]
                        <|
                            el
                                [ centerX
                                , centerY
                                , padding 40
                                , Border.rounded 20
                                , Border.shadow
                                    { offset = ( 0, 3 )
                                    , size = 1
                                    , blur = 4
                                    , color = rgba 0 0 0 0.25
                                    }
                                , Bg.color <| addAlpha 0.85 textColor
                                ]
                            <|
                                formView model.meetupForm

                    Nothing ->
                        none
             ]
                ++ (if isJust model.meetupForm then
                        [ htmlAttribute <| style "overflow-y" "hidden"
                        , htmlAttribute <| style "max-height" "100vh"
                        ]

                    else
                        []
                   )
            )
          <|
            column
                [ height fill
                , width fill
                , spacing 5
                ]
                [ -- Title Bar --
                  column
                    [ width <| px 900
                    , centerX
                    , height fill
                    , padding 20
                    ]
                    [ el [ height <| px 400 ] none
                    , column
                        [ centerX
                        , width fill
                        , padding 40
                        , Border.rounded 20
                        , Border.shadow
                            { offset = ( 0, 3 )
                            , size = 1
                            , blur = 4
                            , color = rgba 0 0 0 0.25
                            }
                        , Bg.color <| addAlpha 0.85 textColor
                        ]
                        [ el
                            [ Font.size 28
                            , Font.bold
                            , padding 15
                            ]
                          <|
                            text "Meetups:"
                        , column
                            [ centerX
                            , width fill
                            , Border.width 2
                            , Border.color <| textColorDark
                            , Border.rounded 5
                            ]
                          <|
                            Array.toList <|
                                Array.indexedMap (meetupToEl <| isJust model.meetupForm) model.meetups
                        , addMeetupButton <| isJust model.meetupForm
                        ]
                    ]
                , el
                    [ padding 15
                    , width fill
                    , Font.color textColor
                    , Font.alignRight
                    , Bg.color primaryColorDark
                    , Border.roundEach
                        { topLeft = 10
                        , topRight = 10
                        , bottomLeft = 0
                        , bottomRight = 0
                        }
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
            ( { model
                | meetups = model.meetups |> Array.push ( meetup, False )
                , meetupForm = Nothing
              }
            , Cmd.none
            )

        OpenMeetupForm ->
            ( { model | meetupForm = Just emptyMeetup }, Cmd.none )

        CloseMeetupForm ->
            ( { model | meetupForm = Nothing }, Cmd.none )

        ToggleExpanded index ->
            ( { model
                | meetups =
                    model.meetups
                        |> Array.set index
                            (Array.get index model.meetups
                                |> Maybe.withDefault ( emptyMeetup, False )
                                |> (\( m, b ) -> ( m, not b ))
                            )
              }
            , Cmd.none
            )

        UpdateForm meetupMsg ->
            case model.meetupForm of
                Nothing ->
                    ( model, Cmd.none )

                Just currentMeetup ->
                    case meetupMsg of
                        Venue venueName ->
                            ( { model
                                | meetupForm =
                                    Just { currentMeetup | venueName = venueName }
                              }
                            , Cmd.none
                            )

                        Street1 street1 ->
                            ( { model
                                | meetupForm =
                                    Just { currentMeetup | street1 = street1 }
                              }
                            , Cmd.none
                            )

                        Street2 street2 ->
                            ( { model
                                | meetupForm =
                                    Just { currentMeetup | street2 = street2 }
                              }
                            , Cmd.none
                            )

                        City city ->
                            ( { model
                                | meetupForm =
                                    Just { currentMeetup | city = city }
                              }
                            , Cmd.none
                            )

                        State state ->
                            ( { model
                                | meetupForm =
                                    Just { currentMeetup | state = state }
                              }
                            , Cmd.none
                            )

                        Zip zip ->
                            ( { model
                                | meetupForm =
                                    Just { currentMeetup | zip = zip }
                              }
                            , Cmd.none
                            )

                        Image img ->
                            ( { model
                                | meetupForm =
                                    Just { currentMeetup | image = img }
                              }
                            , Cmd.none
                            )

                        DateTime dateTime ->
                            ( { model
                                | meetupForm =
                                    Just { currentMeetup | dateTime = dateTime }
                              }
                            , Cmd.none
                            )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    Sub.batch []



-- Helper Functions --


addMeetupButton : Bool -> Element Msg
addMeetupButton isFormVisible =
    let
        label =
            el [ alignRight, paddingXY 5 20 ] <|
                el
                    [ Bg.color primaryColor
                    , Font.color textColor
                    , paddingXY 22 12
                    , Border.rounded 5
                    ]
                <|
                    text
                        "Add Meetup"
    in
    if isFormVisible then
        label

    else
        Input.button [ alignRight, paddingXY 5 10 ]
            { onPress = Just OpenMeetupForm
            , label = label
            }


meetupToEl : Bool -> Int -> ( Meetup, Bool ) -> Element Msg
meetupToEl isFormVisible num ( meetup, expanded ) =
    let
        label =
            case expanded of
                False ->
                    row
                        [ padding 20
                        , spacing 30
                        , width fill
                        , Event.onClick <| ToggleExpanded num
                        , Bg.color <|
                            case modBy 2 num of
                                0 ->
                                    addAlpha 0.5 textColorMid

                                _ ->
                                    addAlpha 0.5 textColor
                        ]
                        [ el [ Font.bold ] <|
                            text <|
                                String.fromInt (num + 1)
                                    ++ "."
                        , el [ width fill, Font.bold ] <| text meetup.venueName
                        , el [] <| text <| meetup.city ++ ", " ++ meetup.state
                        , el [] <| text meetup.dateTime
                        ]

                True ->
                    column
                        [ padding 20
                        , spacing 30
                        , width fill
                        , Event.onClick <| ToggleExpanded num
                        , Bg.color <|
                            case modBy 2 num of
                                0 ->
                                    addAlpha 0.5 textColorMid

                                _ ->
                                    addAlpha 0.5 textColor
                        ]
                        [ el [ Font.bold ] <|
                            text <|
                                String.fromInt (num + 1)
                                    ++ "."
                        , el [ width fill, Font.bold ] <| text meetup.venueName
                        , el [] <| text <| meetup.city ++ ", " ++ meetup.state
                        , el [] <| text meetup.dateTime
                        ]
    in
    if isFormVisible then
        label

    else
        Input.button [ width fill ]
            { onPress = Just <| ToggleExpanded num
            , label = label
            }


textInput : List (Attribute Msg) -> String -> String -> (String -> Msg) -> Element Msg
textInput attrs labelText currentText msgFun =
    Input.text
        attrs
        { onChange = msgFun
        , text = currentText
        , placeholder = Nothing
        , label =
            Input.labelAbove [ paddingXY 10 0, width fill ] <|
                text labelText
        }


formView : Maybe Meetup -> Element Msg
formView meetupForm =
    case meetupForm of
        Just meetup ->
            column [ width fill, spacing 15 ]
                [ textInput
                    [ Border.rounded 5, width fill ]
                    "Venue Name"
                    meetup.venueName
                    (\x -> UpdateForm <| Venue x)
                , textInput
                    [ Border.rounded 5, width fill ]
                    "Street 1"
                    meetup.street1
                    (\x -> UpdateForm <| Street1 x)
                , textInput
                    [ Border.rounded 5, width fill ]
                    "Street 2"
                    meetup.street2
                    (\x -> UpdateForm <| Street2 x)
                , row [ width fill, spacing 10 ]
                    [ textInput
                        [ Border.rounded 5, width <| fillPortion 3 ]
                        "City"
                        meetup.city
                        (\x -> UpdateForm <| City x)
                    , textInput
                        [ Border.rounded 5, width <| fillPortion 1 ]
                        "State"
                        meetup.state
                        (\x -> UpdateForm <| State x)
                    , textInput
                        [ Border.rounded 5, width <| fillPortion 2 ]
                        "Zip Code"
                        meetup.zip
                        (\x -> UpdateForm <| Zip x)
                    ]
                , textInput
                    [ Border.rounded 5, width fill ]
                    "When is it?"
                    meetup.dateTime
                    (\x -> UpdateForm <| DateTime x)
                , row [ alignRight, paddingXY 5 0, spacing 5 ]
                    [ Input.button []
                        { onPress = Just CloseMeetupForm
                        , label =
                            el
                                [ Bg.color textColor
                                , Font.color primaryColor
                                , paddingXY 20 10
                                , Border.rounded 5
                                , Border.width 2
                                ]
                            <|
                                text
                                    "Cancel Form"
                        }
                    , Input.button [ paddingXY 5 0 ]
                        { onPress = Just <| AddMeetup meetup
                        , label =
                            el
                                [ Bg.color primaryColor
                                , Font.color textColor
                                , paddingXY 22 12
                                , Border.rounded 5
                                ]
                            <|
                                text
                                    "Submit Meetup"
                        }
                    ]
                ]

        Nothing ->
            none


isJust : Maybe a -> Bool
isJust x =
    case x of
        Just _ ->
            True

        Nothing ->
            False



-- Color Palette --


primaryColor : Color
primaryColor =
    rgb 0.67 0.09 0.09


textColor : Color
textColor =
    rgb 0.9 0.9 0.9


textColorMid : Color
textColorMid =
    textColor |> mapColor (darken 0.1)


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
