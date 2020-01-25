module Main exposing (..)

-- import Color
-- import Color.Manipulate exposing (darken, lighten)
-- import Html.Attributes exposing (style)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Model exposing (Meetup, MeetupMsg(..), Model, Msg(..))
import Return exposing (Return)
import Style



-- Main App --


main : Program () Model Msg
main =
    Browser.document
        { init = Model.init
        , view = view
        , update = update
        , subscriptions = subs
        }



-- Update --


update : Msg -> Model -> Return Msg Model
update msg model =
    Return.singleton <|
        case msg of
            ChangePage page ->
                { model | page = page }

            AddMeetup meetup ->
                { model
                    | meetups = model.meetups |> Array.push ( meetup, False )
                    , meetupForm = Nothing
                }

            OpenMeetupForm ->
                { model | meetupForm = Just Model.emptyMeetup }

            CloseMeetupForm ->
                { model | meetupForm = Nothing }

            UpdateForm meetupMsg ->
                model.meetupForm
                    |> (Maybe.map <| updateMeetup meetupMsg)
                    |> (\x -> { model | meetupForm = x })

            ToggleExpanded index ->
                { model | meetups = toggleExpanded index model.meetups }


updateMeetup : MeetupMsg -> Meetup -> Meetup
updateMeetup meetupMsg meetup =
    case meetupMsg of
        Venue venueName ->
            { meetup | venueName = venueName }

        Street1 street1 ->
            { meetup | street1 = street1 }

        Street2 street2 ->
            { meetup | street2 = street2 }

        City city ->
            { meetup | city = city }

        State state ->
            { meetup | state = state }

        Zip zip ->
            { meetup | zip = zip }

        Image image ->
            { meetup | image = image }

        DateTime dateTime ->
            { meetup | dateTime = dateTime }


toggleExpanded : Int -> Array ( Meetup, Bool ) -> Array ( Meetup, Bool )
toggleExpanded index meetups =
    let
        toggleCurrent : ( Meetup, Bool ) -> Array ( Meetup, Bool )
        toggleCurrent ( meet, exp ) =
            Array.set index ( meet, not exp ) meetups
    in
    meetups
        |> Array.get index
        |> Maybe.map toggleCurrent
        |> Maybe.withDefault meetups



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    Sub.batch []



-- View --


view : Model -> Browser.Document Msg
view model =
    { title = "Open Source Saturday"
    , body =
        [ Element.layout
            ([ Font.color Style.primaryColor
             , Bg.color Style.textColorDark
             , clip
             , behindContent <|
                image
                    [ centerX
                    , width fill
                    , alpha 0.75
                    , Style.shadow
                    ]
                    { src = "/static/open-source-saturday.jpg"
                    , description = "People at open source saturday!"
                    }
             , inFront <|
                el
                    [ padding 20
                    , width fill
                    , Font.size 30
                    , Font.color Style.textColor
                    , Bg.color Style.primaryColor
                    , alpha 0.8
                    , Style.roundBottomCorners
                    , Style.shadow
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
                                , Style.shadow
                                , Bg.color <|
                                    Style.addAlpha 0.85 Style.textColor
                                ]
                            <|
                                formView model.meetupForm

                    Nothing ->
                        none
             ]
                ++ (if isJust model.meetupForm then
                        Style.disableScroll

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
                [ column
                    [ width <| px 900
                    , centerX
                    , height fill
                    , padding 20
                    ]
                    [ el [ height <| px 440 ] none
                    , column
                        [ centerX
                        , width fill
                        , padding 40
                        , Border.rounded 20
                        , Style.shadow
                        , Bg.color <|
                            Style.addAlpha 0.85 Style.textColor
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
                            , Border.width 1
                            , Border.color <| Style.textColorMid
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
                    , Font.color Style.textColor
                    , Font.alignRight
                    , Bg.color Style.primaryColorDark
                    , Style.roundTopCorners
                    ]
                  <|
                    text "© 2020"
                ]
        ]
    }



-- View Functions --


addMeetupButton : Bool -> Element Msg
addMeetupButton isFormVisible =
    let
        label : Element Msg
        label =
            el [ alignRight, paddingXY 5 20 ] <|
                el
                    [ Bg.color Style.primaryColor
                    , Font.color Style.textColor
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
        label : Element Msg
        label =
            case expanded of
                False ->
                    row
                        [ padding 20
                        , spacing 30
                        , width fill
                        , Event.onClick <| ToggleExpanded num
                        , Style.alternateColors num
                        ]
                        [ el [ Font.bold ] <|
                            text <|
                                String.fromInt (num + 1)
                                    ++ ". "
                                    ++ meetup.venueName
                        , el [] <| text <| meetup.city ++ ", " ++ meetup.state
                        , el
                            [ width fill
                            , Font.alignRight
                            , alignTop
                            ]
                          <|
                            text meetup.dateTime
                        ]

                True ->
                    row
                        [ width fill
                        , spacing 20
                        , Style.alternateColors num
                        ]
                        [ column
                            [ padding 20
                            , spacing 10
                            , width fill
                            , Event.onClick <| ToggleExpanded num
                            ]
                            [ el [ Font.bold ] <|
                                text <|
                                    String.fromInt (num + 1)
                                        ++ ". "
                                        ++ meetup.venueName
                            , el [] <|
                                text <|
                                    meetup.street1
                                        ++ "   "
                                        ++ meetup.street2
                            , el [] <|
                                text <|
                                    meetup.city
                                        ++ ", "
                                        ++ meetup.state
                                        ++ ", "
                                        ++ meetup.zip
                            ]
                        , el
                            [ width fill
                            , padding 20
                            , Font.alignRight
                            , alignTop
                            ]
                          <|
                            text meetup.dateTime
                        , image
                            [ width <| px 300
                            , Font.alignRight
                            , alignTop
                            ]
                            { src = meetup.image
                            , description =
                                "Picture for Open Source Saturday at "
                                    ++ meetup.venueName
                            }

                        --  <|
                        --    text meetup.image
                        ]
    in
    if isFormVisible then
        label

    else
        Input.button
            [ width fill
            , Border.width 1
            , Border.color <| Style.textColorMid
            ]
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
                                [ Bg.color Style.textColor
                                , Font.color Style.primaryColor
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
                                [ Bg.color Style.primaryColor
                                , Font.color Style.textColor
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



-- Helper Functions --


isJust : Maybe a -> Bool
isJust x =
    case x of
        Just _ ->
            True

        Nothing ->
            False
