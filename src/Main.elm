module Main exposing (..)

-- import Color
-- import Color.Manipulate exposing (darken, lighten)
-- import Html.Attributes exposing (style)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onResize)
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
                    | meetups = model.meetups |> pushMeetup meetup
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

            WindowResize newSize ->
                { model | windowSize = newSize }



-- Update Functions --


pushMeetup : Meetup -> Array ( Meetup, Bool ) -> Array ( Meetup, Bool )
pushMeetup meetup =
    Array.push
        ( { meetup | image = "/static/open-source-saturday.jpg" }
        , False
        )


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
    Sub.batch
        [ onResize <| \x y -> WindowResize ( x, y )
        ]



-- View --


view : Model -> Browser.Document Msg
view model =
    { title = "Open Source Saturday"
    , body =
        [ Element.layout
            ([ behindContent <|
                image
                    [ alpha 0.75
                    , centerX
                    , Style.shadow
                    , width fill
                    ]
                    { src = "/static/open-source-saturday.jpg"
                    , description = "People at open source saturday!"
                    }
             , Bg.color Style.textColorDark
             , clip
             , Font.color Style.primaryColor
             , inFront <|
                el
                    [ alpha 0.8
                    , Bg.color Style.primaryColor
                    , Font.color Style.textColor
                    , Font.size 30
                    , padding 20
                    , Style.roundBottomCorners
                    , Style.shadow
                    , width fill
                    ]
                <|
                    text <|
                        "Open Source Saturday    { width = "
                            ++ String.fromInt (Tuple.first model.windowSize)
                            ++ ", height = "
                            ++ String.fromInt (Tuple.second model.windowSize)
                            ++ " }"
             ]
                ++ (if isJust model.meetupForm then
                        [ inFront <|
                            el
                                [ Bg.color <| rgba 0 0 0 0.75
                                , height fill
                                , width fill
                                ]
                            <|
                                el
                                    [ Bg.color <|
                                        Style.addAlpha 0.85 Style.textColor
                                    , Border.rounded 20
                                    , centerX
                                    , centerY
                                    , padding 40
                                    , Style.shadow
                                    ]
                                <|
                                    Maybe.withDefault none <|
                                        Maybe.map formView model.meetupForm
                        ]
                            ++ Style.disableScroll

                    else
                        []
                   )
            )
          <|
            column
                [ height fill
                , spacing 5
                , width fill
                ]
                [ column
                    [ centerX
                    , height fill
                    , width <| px 900
                    , padding 20
                    ]
                    [ el [ height <| px 440 ] none
                    , column
                        [ Bg.color <|
                            Style.addAlpha 0.85 Style.textColor
                        , Border.rounded 20
                        , centerX
                        , padding 40
                        , Style.shadow
                        , width fill
                        ]
                        [ el
                            [ Font.bold
                            , Font.size 28
                            , padding 15
                            ]
                          <|
                            text "Meetups:"
                        , column
                            [ centerX
                            , Style.shadow
                            , width fill
                            ]
                          <|
                            Array.toList <|
                                Array.indexedMap
                                    (meetupToEl <|
                                        isJust model.meetupForm
                                    )
                                    model.meetups
                        , addMeetupButton <| isJust model.meetupForm
                        ]
                    ]
                , el
                    [ Bg.color Style.primaryColorDark
                    , Font.alignRight
                    , Font.color Style.textColor
                    , padding 15
                    , Style.roundTopCorners
                    , width fill
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
                    , Border.rounded 5
                    , Font.color Style.textColor
                    , paddingXY 22 12
                    , Style.shadow
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
            if expanded then
                row
                    [ spacing 20
                    , Style.alternateColors num
                    , width fill
                    ]
                    [ column
                        [ Event.onClick <| ToggleExpanded num
                        , padding 20
                        , spacing 10
                        , width fill
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
                        [ alignTop
                        , Font.alignRight
                        , padding 20
                        , width fill
                        ]
                      <|
                        text meetup.dateTime
                    , image
                        [ alignTop
                        , Font.alignRight
                        , width <| px 300
                        ]
                        { src = meetup.image
                        , description =
                            "Picture for Open Source Saturday at "
                                ++ meetup.venueName
                        }

                    --  <|
                    --    text meetup.image
                    ]

            else
                row
                    [ Event.onClick <| ToggleExpanded num
                    , padding 20
                    , spacing 30
                    , Style.alternateColors num
                    , width fill
                    ]
                    [ el [ Font.bold ] <|
                        text <|
                            String.fromInt (num + 1)
                                ++ ". "
                                ++ meetup.venueName
                    , el [] <| text <| meetup.city ++ ", " ++ meetup.state
                    , el
                        [ alignTop
                        , Font.alignRight
                        , width fill
                        ]
                      <|
                        text meetup.dateTime
                    ]
    in
    if isFormVisible then
        label

    else
        Input.button
            [ Border.color <| Style.textColorMid
            , Border.width 1
            , width fill
            ]
            { onPress = Just <| ToggleExpanded num
            , label = label
            }


textInput : List (Attribute Msg) -> ( String, String, String -> Msg ) -> Element Msg
textInput attrs ( labelText, currentText, msgFun ) =
    Input.text
        (attrs ++ [ Border.rounded 5, Style.shadow ])
        { onChange = msgFun
        , text = currentText
        , placeholder = Nothing
        , label =
            Input.labelAbove [ paddingXY 10 0, width fill ] <|
                text labelText
        }


formView : Meetup -> Element Msg
formView meetup =
    column [ spacing 15, width fill ]
        [ textInput
            [ width fill ]
            ( "Venue Name"
            , meetup.venueName
            , Venue >> UpdateForm
            )
        , textInput
            [ width fill ]
            ( "Street 1"
            , meetup.street1
            , Street1 >> UpdateForm
            )
        , textInput
            [ width fill ]
            ( "Street 2"
            , meetup.street2
            , Street2 >> UpdateForm
            )
        , row [ width fill, spacing 10 ]
            [ textInput
                [ width <| fillPortion 3 ]
                ( "City"
                , meetup.city
                , City >> UpdateForm
                )
            , textInput
                [ width <| fillPortion 1 ]
                ( "State"
                , meetup.state
                , State >> UpdateForm
                )
            , textInput
                [ width <| fillPortion 2 ]
                ( "Zip Code"
                , meetup.zip
                , Zip >> UpdateForm
                )
            ]
        , textInput
            [ width fill ]
            ( "When is it?"
            , meetup.dateTime
            , DateTime >> UpdateForm
            )
        , row [ alignRight, paddingXY 5 0, spacing 15 ]
            [ Input.button []
                { onPress = Just CloseMeetupForm
                , label =
                    el
                        [ Bg.color Style.textColor
                        , Border.rounded 5
                        , Border.width 2
                        , Font.color Style.primaryColor
                        , paddingXY 20 10
                        , Style.shadow
                        ]
                    <|
                        text
                            "Cancel Form"
                }
            , Input.button []
                { onPress = Just <| AddMeetup meetup
                , label =
                    el
                        [ Bg.color Style.primaryColor
                        , Border.rounded 5
                        , Font.color Style.textColor
                        , paddingXY 22 12
                        , Style.shadow
                        ]
                    <|
                        text
                            "Submit Meetup"
                }
            ]
        ]



-- Helper Functions --


isJust : Maybe a -> Bool
isJust x =
    case x of
        Just _ ->
            True

        Nothing ->
            False
