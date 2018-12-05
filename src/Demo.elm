import Browser
import Date
import DatePicker
import Html exposing (..)

type alias Flags = {}
type alias DateRange =
    { startDate: Maybe Date.Date
    , startDatePicker: DatePicker.DatePicker
    , endDate: Maybe Date.Date
    , endDatePicker: DatePicker.DatePicker
    }

type alias Model =
    { createdOnPicker: DateRange }

type DateRangeField
    = ChangingStartDate
    | ChangingEndDate

type Msg 
    = ChangeCreatedOnRange DateRangeField DatePicker.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeCreatedOnRange field innerMsg ->
            let
                picker = if field == ChangingStartDate then model.createdOnPicker.startDatePicker else model.createdOnPicker.endDatePicker
                settings = if field == ChangingStartDate then (startSettings model.createdOnPicker.endDate) else (endSettings model.createdOnPicker.startDate)
                (newPicker, dateEvent) =
                    DatePicker.update settings innerMsg picker
                newDate =
                    case dateEvent of
                        DatePicker.Picked changedDate ->
                            Just changedDate
                        _ ->
                            if field == ChangingStartDate then
                                model.createdOnPicker.startDate
                            else
                                model.createdOnPicker.endDate
                oldPickerState = model.createdOnPicker
                freshPickerState =
                    if field == ChangingStartDate then
                        { oldPickerState | startDate = newDate, startDatePicker = newPicker }
                    else
                        { oldPickerState | endDate = newDate, endDatePicker = newPicker }
            in
                ({ model | createdOnPicker = freshPickerState }, Cmd.none)

commonSettings : DatePicker.Settings
commonSettings =
    DatePicker.defaultSettings

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
                                || commonSettings.isDisabled d -- ????
    in
        { commonSettings | placeholder = "Pick a Start Date", isDisabled = isDisabled }

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
        { commonSettings | placeholder = "Pick an End Date", isDisabled = isDisabled }

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
            [ Cmd.map (ChangeCreatedOnRange ChangingStartDate) startDatePickerCmd
            , Cmd.map (ChangeCreatedOnRange ChangingEndDate) endDatePickerCmd
            ]
        )

view : Model -> Html Msg
view model =
    viewPicker model.createdOnPicker

viewPicker : DateRange -> Html Msg
viewPicker range =
    div []
        [ viewRange range.startDate range.endDate
        , DatePicker.view range.startDate (startSettings range.endDate) range.startDatePicker
            |> Html.map (ChangeCreatedOnRange ChangingStartDate)
        , DatePicker.view range.endDate (endSettings range.startDate) range.endDatePicker
            |> Html.map (ChangeCreatedOnRange ChangingEndDate)
        ]

viewRange : Maybe Date.Date -> Maybe Date.Date -> Html Msg
viewRange start end =
    case (start, end) of
        (Nothing, Nothing) ->
            h1 [] [ text "Pick dates"] 
        (Just s, Nothing) ->
            h1 [] [ text <| formatDate s ++ " - Pick end date" ]
        (Nothing, Just e) ->
            h1 [] [ text <| "Pick start date - " ++ formatDate e ]
        (Just s, Just e) ->
            h1 [] [ text <| formatDate s ++ " - " ++ formatDate e ]

formatDate : Date.Date -> String
formatDate date =
    Date.format "MMM dd, yyyy" date

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none) 
        , view = view
        }
