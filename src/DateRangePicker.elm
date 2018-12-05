module DateRangePicker exposing
    ( DateRangeField(..)
    , DateRangePicker
    , init
    , updateRangePicker
    , viewRangePicker
    )

import Date exposing (Date)
import DatePicker exposing (DatePicker)
import Html exposing (..)


type DateRangeField
    = ChangingLowerBound
    | ChangingUpperBound


type alias DateRangePicker =
    { startDate : Maybe Date
    , startDatePicker : DatePicker
    , endDate : Maybe Date
    , endDatePicker : DatePicker
    }


init : ( DateRangePicker, Cmd DatePicker.Msg, Cmd DatePicker.Msg )
init =
    let
        ( startDatePicker, startDatePickerCmd ) =
            DatePicker.init

        ( endDatePicker, endDatePickerCmd ) =
            DatePicker.init
    in
    ( { startDate = Nothing
      , startDatePicker = startDatePicker
      , endDate = Nothing
      , endDatePicker = endDatePicker
      }
    , startDatePickerCmd
    , endDatePickerCmd
      -- TODO: better as a record?
    )


viewRange : Maybe Date.Date -> Maybe Date.Date -> Html msg
viewRange start end =
    case ( start, end ) of
        ( Nothing, Nothing ) ->
            h1 [] [ text "Pick dates" ]

        ( Just s, Nothing ) ->
            h1 [] [ text <| formatDate s ++ " - Pick end date" ]

        ( Nothing, Just e ) ->
            h1 [] [ text <| "Pick start date - " ++ formatDate e ]

        ( Just s, Just e ) ->
            h1 [] [ text <| formatDate s ++ " - " ++ formatDate e ]


viewRangePicker : (DatePicker.Msg -> msg) -> (DatePicker.Msg -> msg) -> DateRangePicker -> Html msg
viewRangePicker onChangeStartDate onChangeEndDate range =
    div []
        [ viewRange range.startDate range.endDate
        , DatePicker.view range.startDate (startSettings range.endDate) range.startDatePicker
            |> Html.map onChangeStartDate
        , DatePicker.view range.endDate (endSettings range.startDate) range.endDatePicker
            |> Html.map onChangeEndDate
        ]


updateRangePicker : DateRangeField -> DatePicker.Msg -> DateRangePicker -> DateRangePicker
updateRangePicker field innerMsg target =
    let
        picker =
            if field == ChangingLowerBound then
                target.startDatePicker

            else
                target.endDatePicker

        settings =
            if field == ChangingLowerBound then
                startSettings target.endDate

            else
                endSettings target.startDate

        ( newPicker, dateEvent ) =
            DatePicker.update settings innerMsg picker

        newDate =
            case dateEvent of
                DatePicker.Picked changedDate ->
                    Just changedDate

                _ ->
                    if field == ChangingLowerBound then
                        target.startDate

                    else
                        target.endDate
    in
    if field == ChangingLowerBound then
        { target | startDate = newDate, startDatePicker = newPicker }

    else
        { target | endDate = newDate, endDatePicker = newPicker }

-- Private settings
commonSettings : DatePicker.Settings
commonSettings =
    let
        settings = DatePicker.defaultSettings
    in
        { settings
            | inputClassList = [ ("form-control", True) ]
            , placeholder = "mm/dd/yyyy"
            , dateFormatter = Date.format "MM/dd/yyyy"
        }


startSettings : Maybe Date.Date -> DatePicker.Settings
startSettings endDate =
    -- disable the dates "after" the end date, because they aren't a valid
    -- choice for a start date, obviously.
    let
        isDisabled =
            case endDate of
                Nothing ->
                    commonSettings.isDisabled

                Just date ->
                    \d ->
                        Date.toRataDie d
                            > Date.toRataDie date
                            || commonSettings.isDisabled d

        -- ????
    in
    { commonSettings | isDisabled = isDisabled }


endSettings : Maybe Date.Date -> DatePicker.Settings
endSettings startDate =
    let
        isDisabled =
            case startDate of
                Nothing ->
                    commonSettings.isDisabled

                Just date ->
                    \d ->
                        Date.toRataDie d
                            < Date.toRataDie date
                            || commonSettings.isDisabled d
    in
    { commonSettings | isDisabled = isDisabled }


formatDate : Date.Date -> String
formatDate date =
    Date.format "MMM dd, yyyy" date
