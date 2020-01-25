module Model exposing (..)

import Array exposing (Array)



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
            Array.fromList <|
                List.repeat 4
                    ( { street1 = "2780 La Mirada Dr."
                      , street2 = "Suite E"
                      , city = "Vista"
                      , state = "CA"
                      , zip = "92081"
                      , venueName = "Open Source Maker Labs"
                      , image = "/static/open-source-saturday.jpg"
                      , dateTime = "now"
                      }
                    , False
                    )
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
