import Browser
import Date
import DatePicker
import DateRangePicker
import Html exposing (..)

type alias Flags = {}

type alias Model =
    { createdOnPicker: DateRangePicker.DateRangePicker }

type Msg 
    = ChangeCreatedOnRange DateRangePicker.DateRangeField DatePicker.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeCreatedOnRange field innerMsg ->
            let
                freshPickerState =
                    DateRangePicker.updateRangePicker field innerMsg model.createdOnPicker
            in
                ({ model | createdOnPicker = freshPickerState }, Cmd.none)


init : Flags -> (Model, Cmd Msg)
init flags =
    let
        (startDatePicker, startDatePickerCmd) = DatePicker.init
        (endDatePicker, endDatePickerCmd) = DatePicker.init
    in
        ({ createdOnPicker =
            { startDate = Nothing
            , startDatePicker = startDatePicker
            , endDate = Nothing
            , endDatePicker = endDatePicker
            }
        },
        Cmd.batch
            [ Cmd.map (ChangeCreatedOnRange DateRangePicker.ChangingStartDate) startDatePickerCmd
            , Cmd.map (ChangeCreatedOnRange DateRangePicker.ChangingEndDate) endDatePickerCmd
            ]
        )

view : Model -> Html Msg
view model =
    DateRangePicker.viewRangePicker
        (ChangeCreatedOnRange DateRangePicker.ChangingStartDate)
        (ChangeCreatedOnRange DateRangePicker.ChangingEndDate)
        model.createdOnPicker

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none) 
        , view = view
        }
