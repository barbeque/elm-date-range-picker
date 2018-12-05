import Browser
import Date
import DatePicker
import Html exposing (..)

type alias Flags = {}
type alias Model =
    { startDate: Maybe Date.Date
    , startDatePicker: DatePicker.DatePicker
    , endDate: Maybe Date.Date
    , endDatePicker: DatePicker.DatePicker
    }

type Msg 
    = ToStartDatePicker DatePicker.Msg
    | ToEndDatePicker DatePicker.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToStartDatePicker innerMsg ->
            let
                (newPicker, dateEvent) =
                    DatePicker.update (startSettings model.endDate) innerMsg model.startDatePicker
                newDate =
                    case dateEvent of
                        DatePicker.Picked changedDate ->
                            Just changedDate
                        _ ->
                            model.startDate
            in
                ({ model
                    | startDate = newDate 
                    , startDatePicker = newPicker
                }, Cmd.none )
        ToEndDatePicker innerMsg ->
            let
                (newPicker, dateEvent) =
                    DatePicker.update (endSettings model.startDate) innerMsg model.endDatePicker
                newDate =
                    case dateEvent of
                        DatePicker.Picked changedDate ->
                            Just changedDate
                        _ ->
                            model.startDate
            in
                ({ model
                    | endDate = newDate 
                    , endDatePicker = newPicker
                }, Cmd.none )

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
        ({ startDate = Nothing
        , startDatePicker = startDatePicker
        , endDate = Nothing
        , endDatePicker = endDatePicker
        },
        Cmd.batch
            [ Cmd.map ToStartDatePicker startDatePickerCmd
            , Cmd.map ToEndDatePicker endDatePickerCmd
            ]
        )

view : Model -> Html Msg
view model =
    div []
        [ viewRange model.startDate model.endDate
        , DatePicker.view model.startDate (startSettings model.endDate) model.startDatePicker
            |> Html.map ToStartDatePicker
        , DatePicker.view model.endDate (endSettings model.startDate) model.endDatePicker
            |> Html.map ToEndDatePicker
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
