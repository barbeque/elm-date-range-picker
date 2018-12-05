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
        (createdOnPicker, updateCreatedOnStartCmd, updateCreatedOnEndCmd) =
            DateRangePicker.init
    in
        ({ createdOnPicker = createdOnPicker
        },
        Cmd.batch
            -- ugly, but it's the best i can think of right now
            [ Cmd.map (ChangeCreatedOnRange DateRangePicker.ChangingStartDate) updateCreatedOnStartCmd
            , Cmd.map (ChangeCreatedOnRange DateRangePicker.ChangingEndDate) updateCreatedOnEndCmd
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
