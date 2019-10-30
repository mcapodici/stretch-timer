module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewLink)

import Array
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import ExerciseProgram exposing (Exercise, ExerciseId, ExerciseProgram)
import Html exposing (Attribute, Html, a, button, div, fieldset, h1, h2, i, input, label, li, p, span, table, td, text, th, tr)
import Html.Attributes exposing (attribute, class, colspan, href, id, pattern, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Ports.LocalStorage as LocalStorage
import Ports.Sound exposing (play)
import Progress exposing (ExercisePart(..), Progress, SoundResult(..))
import Regex
import Task
import Time
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        }



-- MODEL


{-| The state as you are filling out the form. Strings are needed because the fields
are free text until the user saves the form, when they are converted into the Exercise.
-}
type alias ExerciseForm =
    { name : String
    , duration : String
    , restDuration : String
    , repetitions : String
    , prepDuration : String
    }


{-| The current state of expanding exerises. Either nothing is expanded, or a particular
exercise is expanded
-}
type ExpanderState
    = NothingExpanded
    | Expanded ExerciseId


{-| Main app navigation (where you are in the app)
-}
type Navigation
    = Home ExpanderState
    | Edit ExerciseId ExerciseForm
    | New ExerciseForm
    | Run


{-| Represents an prompt to ask if you are sure, along with the action (Msg) to
perform if the user says yes
-}
type alias AreYouSurePrompt =
    { prompt : String
    , action : Msg
    }


type alias Model =
    { navigation : Navigation
    , program : ExerciseProgram
    , time : Time.Posix
    , progress : Progress
    , isPaused : Bool
    , areYouSurePrompt : Maybe AreYouSurePrompt
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { navigation = Home NothingExpanded
      , program = ExerciseProgram.new
      , time = Time.millisToPosix 0
      , isPaused = True
      , progress = Progress.finish
      , areYouSurePrompt = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddExercise
    | SaveNewExercise
    | CancelNewExercise
    | SaveEditExercise
    | CancelEditExercise
    | DeleteExercise ExerciseId
    | SetFormField FormMsg
    | EditExercise ExerciseId
    | ExpandContractExercise ExerciseId
    | NoOp
    | LoadProgram ExerciseProgram
    | Tick Time.Posix
    | StartRoutine
    | ContinueRoutine
    | GoHome
    | PauseOrResumeRoutine
    | PreviousExercise
    | NextExercise
    | PreviousExerciseRep
    | NextExerciseRep
    | MoveExerciseUp
    | MoveExerciseDown
    | AreYouSure String Msg
    | AreYouSureNo
    | AreYouSureYes Msg
    | StartAtExercise ExerciseId


type FormMsg
    = SetName String
    | SetDuration String
    | SetRestDuration String
    | SetRepetitions String
    | SetPrepDuration String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        AddExercise ->
            ( { model | navigation = New (initExerciseForm newExerciseDefaults) }, focusName )

        EditExercise id ->
            case ExerciseProgram.get id model.program of
                Just exercise ->
                    ( { model | navigation = Edit id (initExerciseForm exercise) }, focusName )

                Nothing ->
                    ( model, Cmd.none )

        SetFormField formMsg ->
            case model.navigation of
                New form ->
                    ( { model | navigation = New (updateExerciseForm formMsg form) }, Cmd.none )

                Edit id form ->
                    ( { model | navigation = Edit id (updateExerciseForm formMsg form) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SaveNewExercise ->
            case model.navigation of
                New form ->
                    let
                        newProgram =
                            ExerciseProgram.add (parseExerciseForm form) model.program
                    in
                    ( { model
                        | navigation = Home NothingExpanded
                        , program = newProgram
                      }
                    , cache newProgram
                    )

                _ ->
                    ( model, Cmd.none )

        CancelNewExercise ->
            ( { model | navigation = Home NothingExpanded }, Cmd.none )

        SaveEditExercise ->
            case model.navigation of
                Edit id form ->
                    let
                        newProgram =
                            ExerciseProgram.update id (parseExerciseForm form) model.program
                    in
                    ( { model
                        | navigation = Home NothingExpanded
                        , program = newProgram
                      }
                    , cache newProgram
                    )

                _ ->
                    ( model, Cmd.none )

        CancelEditExercise ->
            ( { model | navigation = Home NothingExpanded }, Cmd.none )

        DeleteExercise id ->
            let
                newProgram =
                    ExerciseProgram.remove id model.program
            in
            ( { model | navigation = Home NothingExpanded, program = newProgram }, cache newProgram )

        LoadProgram program ->
            ( { model | program = program }, Cmd.none )

        Tick newTime ->
            let
                differenceMillis =
                    Time.posixToMillis newTime - Time.posixToMillis model.time

                modelWithNewTime =
                    { model | time = newTime }

                newProgress =
                    if model.isPaused then
                        model.progress

                    else
                        Progress.incrementByMilliseconds model.program differenceMillis model.progress
            in
            ( { modelWithNewTime | progress = newProgress }
            , soundForProgressChange model.progress newProgress
            )

        StartRoutine ->
            ( { model
                | navigation = Run
                , isPaused = False
                , progress = Progress.start
              }
            , play "prepare"
            )

        StartAtExercise id ->
            ( { model
                | navigation = Run
                , isPaused = False
                , progress = Progress.goToExercise id
              }
            , play "prepare"
            )

        ContinueRoutine ->
            ( { model
                | navigation = Run
                , isPaused = False
              }
            , Cmd.none
            )

        GoHome ->
            ( { model | navigation = Home NothingExpanded, isPaused = True }
            , Cmd.none
            )

        PauseOrResumeRoutine ->
            let
                newIsPaused =
                    not model.isPaused
            in
            ( { model | isPaused = newIsPaused }
            , if not newIsPaused then
                soundToCmd (Progress.getToneForResume model.progress)

              else
                Cmd.none
            )

        PreviousExercise ->
            let
                newProgress =
                    Progress.goToPreviousExercise model.program model.progress
            in
            ( { model | progress = newProgress, isPaused = True }
            , Cmd.none
            )

        NextExercise ->
            let
                newProgress =
                    Progress.goToNextExercise model.program model.progress
            in
            ( { model | progress = newProgress, isPaused = True }
            , Cmd.none
            )

        PreviousExerciseRep ->
            let
                newProgress =
                    Progress.goToPreviousExerciseRep model.program model.progress
            in
            ( { model | progress = newProgress, isPaused = True }
            , Cmd.none
            )

        NextExerciseRep ->
            let
                newProgress =
                    Progress.goToNextExerciseRep model.program model.progress
            in
            ( { model | progress = newProgress, isPaused = True }
            , Cmd.none
            )

        ExpandContractExercise exerciseId ->
            ( { model
                | navigation =
                    case model.navigation of
                        Home (Expanded id) ->
                            if id == exerciseId then
                                -- Contract the exercise again
                                Home NothingExpanded

                            else
                                Home (Expanded exerciseId)

                        _ ->
                            Home (Expanded exerciseId)
              }
            , Cmd.none
            )

        MoveExerciseUp ->
            case model.navigation of
                Home (Expanded exerciseId) ->
                    if exerciseId > 0 then
                        let
                            newProgram =
                                ExerciseProgram.swap (exerciseId - 1) exerciseId model.program
                        in
                        ( { model
                            | program = newProgram
                            , navigation = Home (Expanded (exerciseId - 1))
                          }
                        , cache newProgram
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MoveExerciseDown ->
            case model.navigation of
                Home (Expanded exerciseId) ->
                    if exerciseId < (ExerciseProgram.length model.program - 1) then
                        let
                            newProgram =
                                ExerciseProgram.swap exerciseId (exerciseId + 1) model.program
                        in
                        ( { model
                            | program = newProgram
                            , navigation = Home (Expanded (exerciseId + 1))
                          }
                        , cache newProgram
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AreYouSure prompt action ->
            ( { model | areYouSurePrompt = Just { prompt = prompt, action = action } }, Cmd.none )

        AreYouSureNo ->
            ( { model | areYouSurePrompt = Nothing }, Cmd.none )

        AreYouSureYes action ->
            update action { model | areYouSurePrompt = Nothing }


soundForProgressChange : Progress -> Progress -> Cmd msg
soundForProgressChange oldProgress newProgress =
    soundToCmd (Progress.getToneForTransition oldProgress newProgress)


{-| Sounds taken from <https://soundoftext.com/>
-}
soundToCmd : SoundResult -> Cmd msg
soundToCmd result =
    case result of
        SoundGo ->
            play "go"

        SoundRest ->
            play "rest"

        SoundPrepare ->
            play "prepare"

        SoundAllFinished ->
            play "allfinished"

        NoSound ->
            Cmd.none


regexNotADigit =
    Regex.fromString "[^0-9]" |> Maybe.withDefault Regex.never


removeNonDigits : String -> String
removeNonDigits =
    Regex.replace regexNotADigit (always "")


updateExerciseForm : FormMsg -> ExerciseForm -> ExerciseForm
updateExerciseForm msg model =
    case msg of
        SetName s ->
            { model | name = s }

        SetDuration s ->
            { model | duration = removeNonDigits s }

        SetRestDuration s ->
            { model | restDuration = removeNonDigits s }

        SetRepetitions s ->
            { model | repetitions = removeNonDigits s }

        SetPrepDuration s ->
            { model | prepDuration = removeNonDigits s }


newExerciseDefaults =
    { name = "", duration = 25, restDuration = 5, repetitions = 3, prepDuration = 10 }


initExerciseForm : Exercise -> ExerciseForm
initExerciseForm ex =
    { name = ex.name
    , duration = String.fromInt ex.duration
    , restDuration = String.fromInt ex.restDuration
    , repetitions = String.fromInt ex.repetitions
    , prepDuration = String.fromInt ex.prepDuration
    }


parseExerciseForm : ExerciseForm -> Exercise
parseExerciseForm ex =
    { name = ex.name
    , duration = String.toInt ex.duration |> Maybe.withDefault 0 |> max 1
    , restDuration = String.toInt ex.restDuration |> Maybe.withDefault 0 |> max 0
    , repetitions = String.toInt ex.repetitions |> Maybe.withDefault 0 |> max 1
    , prepDuration = String.toInt ex.prepDuration |> Maybe.withDefault 0 |> max 0
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onLocalStorageChange
        , Time.every 10 Tick
        ]


onLocalStorageChange : Sub Msg
onLocalStorageChange =
    LocalStorage.onChange
        (\json ->
            D.decodeString ExerciseProgram.decode json |> Result.map LoadProgram |> Result.withDefault NoOp
        )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.areYouSurePrompt of
        Just prompt ->
            { title = "Home"
            , body = [ viewAreYouSurePrompt prompt ]
            }

        Nothing ->
            case model.navigation of
                Home expander ->
                    viewHome expander (Progress.isFinished model.progress) model.program

                New form ->
                    viewAddExercise form

                Edit id form ->
                    viewEditExercise id form

                Run ->
                    viewRun model.program model.isPaused model.progress


viewAreYouSurePrompt : AreYouSurePrompt -> Html Msg
viewAreYouSurePrompt prompt =
    div [ class "container" ]
        [ h1 [] [ text prompt.prompt ]
        , button [ onClick (AreYouSureYes prompt.action), class "button-clear" ] [ text "Yes" ]
        , text " "
        , button [ onClick AreYouSureNo ] [ text "No" ]
        ]


viewRun : ExerciseProgram -> Bool -> Progress -> Browser.Document Msg
viewRun program isPaused progress =
    { title = "Run"
    , body =
        [ div [ class "" ]
            [ div [ class "container align-right" ]
                [ button [ onClick GoHome, class "button" ] [ text "Home" ]
                ]
            , div []
                ((case Progress.getProgressInfo progress of
                    Nothing ->
                        [ div [ class "exercise-name" ]
                            [ text <| "Finished"
                            ]
                        ]

                    Just p ->
                        case ExerciseProgram.get p.exerciseId program of
                            Just exercise ->
                                [ div [ class "exercise-name" ]
                                    [ text <|
                                        case p.exercisePart of
                                            Prep ->
                                                "Get ready for " ++ exercise.name

                                            Rep rep ->
                                                if rep.isRest then
                                                    "Resting"

                                                else
                                                    exercise.name ++ " " ++ String.fromInt (rep.index + 1) ++ " of " ++ String.fromInt exercise.repetitions
                                    ]
                                , div [ class "exercise-remaining" ]
                                    (let
                                        remainingMillis =
                                            Progress.remainingPartTimeMilliseconds program progress

                                        remainingPct =
                                            Progress.remainingPartTimePercent program progress
                                     in
                                     [ div [ class "exercise-remaining-bar", style "width" (String.fromFloat remainingPct ++ "%") ] []
                                     , span [ class "exercise-remaining-time-text" ] [ text (String.fromInt ((remainingMillis + 999) // 1000)) ]
                                     ]
                                    )
                                ]

                            -- Shouldn't occur; treat as finished
                            Nothing ->
                                [ h2 [] [ text "Finished" ] ]
                 )
                    ++ [ div [ class "play-buttons" ]
                            [ materialButton PreviousExercise "keyboard_arrow_up"
                            , materialButton PreviousExerciseRep "skip_previous"
                            , materialButton PauseOrResumeRoutine
                                (if isPaused then
                                    "play_arrow"

                                 else
                                    "pause"
                                )
                            , materialButton NextExerciseRep "skip_next"
                            , materialButton NextExercise "keyboard_arrow_down"
                            ]
                       ]
                )
            ]
        ]
    }


materialButton : msg -> String -> Html msg
materialButton message icon_name =
    button
        [ class "button", onClick message ]
        [ materialIcon icon_name
        ]


materialIcon : String -> Html msg
materialIcon icon_name =
    i [ class "material-icons" ]
        [ text icon_name
        ]


viewHome : ExpanderState -> Bool -> ExerciseProgram -> Browser.Document Msg
viewHome expander isFinished program =
    { title = "Home"
    , body =
        [ div [ class "container" ]
            (if ExerciseProgram.isEmpty program then
                renderWelcome

             else
                renderProgram expander isFinished program
            )
        ]
    }


exerciseAddEditForm : ExerciseForm -> Html Msg
exerciseAddEditForm form =
    fieldset []
        [ label [] [ text "Name" ]
        , input [ id "form-name", onInput (\x -> SetFormField (SetName x)), type_ "text", value form.name ] []
        , label [] [ text "Duration (s)" ]
        , numericInput [ onInput (\x -> SetFormField (SetDuration x)), value form.duration ] []
        , label [] [ text "Rest duration (s)" ]
        , numericInput [ onInput (\x -> SetFormField (SetRestDuration x)), value form.restDuration ] []
        , label [] [ text "Repetitions" ]
        , numericInput [ onInput (\x -> SetFormField (SetRepetitions x)), value form.repetitions ] []
        , label [] [ text "Prepare time (s)" ]
        , numericInput [ onInput (\x -> SetFormField (SetPrepDuration x)), value form.prepDuration ] []
        ]


numericInputAttributes : List (Attribute msg)
numericInputAttributes =
    [ type_ "text", pattern "[0-9]*", attribute "inputmode" "decimal" ]


numericInput : List (Attribute msg) -> List (Html msg) -> Html msg
numericInput attrs htmls =
    input (attrs ++ numericInputAttributes) htmls


viewAddExercise : ExerciseForm -> Browser.Document Msg
viewAddExercise form =
    { title = "Add Stretch"
    , body =
        [ div [ class "container" ]
            [ h1 []
                [ text "Add Stretch." ]
            , p [] [ text "I need to know some basic things about this stretch. Fill out this form and click Save to add this stretch to your program." ]
            , exerciseAddEditForm form
            , div [ class "align-right" ]
                [ button [ onClick CancelNewExercise, class "button-clear" ] [ text "Cancel" ]
                , button [ onClick SaveNewExercise, class "button" ] [ text "Save" ]
                ]
            ]
        ]
    }


viewEditExercise : ExerciseId -> ExerciseForm -> Browser.Document Msg
viewEditExercise id form =
    { title = "Edit Stretch"
    , body =
        [ div [ class "container" ]
            [ h1 []
                [ text "Edit Stretch." ]
            , p [] [ text "I need to know some basic things about this stretch. Fill out this form and click Save to add this stretch to your program." ]
            , exerciseAddEditForm form
            , div [ class "align-right" ]
                [ button [ onClick CancelEditExercise, class "button-clear" ] [ text "Cancel" ]
                , button [ onClick SaveEditExercise, class "button" ] [ text "Save" ]
                ]
            ]
        ]
    }


renderProgram : ExpanderState -> Bool -> ExerciseProgram -> List (Html Msg)
renderProgram expander isFinished exercises =
    [ h1 [] [ text "Your Routine" ]
    , if isFinished then
        text ""

      else
        button [ onClick ContinueRoutine ] [ text "Continue" ]
    , text " "
    , if isFinished then
        button [ onClick StartRoutine ] [ text "Start" ]

      else
        button [ onClick (AreYouSure "Restart unfinished routine?" StartRoutine) ] [ text "Restart" ]
    , table []
        ([ tr []
            [ th [] [ text "Stretch Name" ]
            , th [] []
            ]
         ]
            ++ List.concat
                (List.indexedMap
                    (\id exercise ->
                        [ tr []
                            [ td []
                                [ text exercise.name
                                ]
                            , td [ class "align-right" ] [ button [ onClick (ExpandContractExercise id) ] [ text "..." ] ]
                            ]
                        , if expander == Expanded id then
                            tr []
                                [ td [ colspan 2 ]
                                    [ button [ onClick (StartAtExercise id), class "button" ] [ text "Start Here" ]
                                    , text " "
                                    , button [ onClick (EditExercise id) ] [ text "Edit" ]
                                    , text " "
                                    , button [ onClick (AreYouSure ("Delete exercise " ++ exercise.name ++ "?") (DeleteExercise id)), class "button" ] [ text "Delete" ]
                                    , text " "
                                    , button [ onClick MoveExerciseDown, class "button" ] [ text "Move Down" ]
                                    , text " "
                                    , button [ onClick MoveExerciseUp, class "button" ] [ text "Move Up" ]
                                    , text " "
                                    ]
                                ]

                          else
                            text ""
                        ]
                    )
                    (ExerciseProgram.toList exercises)
                )
        )
    , button [ onClick AddExercise ] [ text "Add another stretch" ]
    ]


renderWelcome : List (Html Msg)
renderWelcome =
    [ h1 [] [ text "Stretch Timer." ]
    , p
        []
        [ span [ style "font-weight" "bold" ] [ text "Hello, " ]
        , text "I am here to choreograph your stretching routine. Tell me what stretches you want to do and for how long."
        ]
    , button [ onClick AddExercise ] [ text "Add your first stretch..." ]
    ]


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


focusName : Cmd Msg
focusName =
    Task.attempt (\_ -> NoOp) (Dom.focus "form-name")


cache : ExerciseProgram -> Cmd msg
cache ex =
    LocalStorage.cache (E.encode 0 (ExerciseProgram.encode ex))
