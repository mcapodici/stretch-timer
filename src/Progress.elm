module Progress exposing (ExercisePart(..), Progress, ProgressInfo, SoundResult(..), finish, getProgressInfo, getToneForResume, getToneForTransition, goToExercise, goToNextExercise, goToNextExerciseRep, goToPreviousExercise, goToPreviousExerciseRep, incrementByMilliseconds, isFinished, remainingPartTimeMilliseconds, remainingPartTimePercent, start)

import ExerciseProgram exposing (Exercise, ExerciseId, ExerciseProgram, get)
import Json.Decode as D
import Json.Encode as E


type Progress
    = Finished
    | Progress ProgressInfo


type ExercisePart
    = Prep
    | Rep
        { index : Int
        , isRest : Bool
        }


type alias ProgressInfo =
    { exerciseId : ExerciseId
    , exercisePart : ExercisePart
    , timeMilliseconds : Int
    }


type SoundResult
    = SoundGo
    | SoundRest
    | SoundPrepare
    | SoundAllFinished
    | NoSound



-- Navigational Aids
--
-- These are for when you need to get to a specific part of the exercise program


start : Progress
start =
    Progress
        { exerciseId = 0
        , exercisePart = Prep
        , timeMilliseconds = 0
        }


goToExercise : ExerciseId -> Progress
goToExercise id =
    Progress
        { exerciseId = id
        , exercisePart = Prep
        , timeMilliseconds = 0
        }


finish : Progress
finish =
    Finished



-- Remaining Time Functions
--
-- These provide the data for showing the user the remaining time.


remainingPartTimeMilliseconds : ExerciseProgram -> Progress -> Int
remainingPartTimeMilliseconds program progress =
    case currentExercise_ program progress of
        Nothing ->
            0

        Just ( exercise, p ) ->
            max 0 (calculateCurrentExercisePartTimeMilliseconds_ p exercise - p.timeMilliseconds)


remainingPartTimePercent : ExerciseProgram -> Progress -> Float
remainingPartTimePercent program progress =
    case currentExercise_ program progress of
        Nothing ->
            0

        Just ( exercise, p ) ->
            let
                totalLength =
                    calculateCurrentExercisePartTimeMilliseconds_ p exercise
            in
            if totalLength == 0 then
                0

            else
                max 0 (toFloat ((totalLength - p.timeMilliseconds) * 100) / toFloat totalLength)



-- Queries


isFinished : Progress -> Bool
isFinished progress =
    case progress of
        Finished ->
            True

        Progress _ ->
            False


{-| Gets the progress info, if the progress is not finished -
-}
getProgressInfo : Progress -> Maybe ProgressInfo
getProgressInfo progress =
    case progress of
        Finished ->
            Nothing

        Progress p ->
            Just p



-- Sound queries
--
-- These are for knowing what noise to make. This file doesn't deal with
-- sound generation directly but just describes the approriate noise as a SoundResult


getToneForTransition : Progress -> Progress -> SoundResult
getToneForTransition from to =
    case ( from, to ) of
        ( Progress p1, Progress p2 ) ->
            if
                p1.exerciseId
                    /= p2.exerciseId
                    || p1.exercisePart
                    /= p2.exercisePart
            then
                case p2.exercisePart of
                    Prep ->
                        SoundPrepare

                    Rep rep ->
                        if rep.isRest then
                            SoundRest

                        else
                            SoundGo

            else
                NoSound

        ( Progress p1, Finished ) ->
            SoundAllFinished

        _ ->
            NoSound


getToneForResume : Progress -> SoundResult
getToneForResume progress =
    case progress of
        Progress p ->
            if p.timeMilliseconds == 0 then
                case p.exercisePart of
                    Prep ->
                        SoundPrepare

                    Rep rep ->
                        if rep.isRest then
                            SoundRest

                        else
                            SoundGo

            else
                NoSound

        _ ->
            NoSound



-- Transitions
--
-- These functions help change the progress releative to the current progress


incrementByMilliseconds : ExerciseProgram -> Int -> Progress -> Progress
incrementByMilliseconds program milliseconds progress =
    case currentExercise_ program progress of
        Nothing ->
            Finished

        Just ( exercise, p ) ->
            let
                millisecondsChecked =
                    max 0 milliseconds

                -- A proposed new progress value, but which might overrun if the current exercise part
                -- has been completed, so this is checked before returning.
                newProgressWithPossibleOverrun =
                    { p | timeMilliseconds = p.timeMilliseconds + millisecondsChecked }

                -- Time time available for this exercise part in total
                availableTime =
                    calculateCurrentExercisePartTimeMilliseconds_ p exercise

                -- The amount of time we have overrun the current exercise
                overrunTime =
                    newProgressWithPossibleOverrun.timeMilliseconds - availableTime
            in
            if overrunTime >= 0 then
                -- This takes us to a new exercise part, so move over to that, and repeat the adding process with
                -- the remaining time to add.
                let
                    nextExercise =
                        goToNextPartOfExercise_ exercise newProgressWithPossibleOverrun
                in
                incrementByMilliseconds program overrunTime (Progress nextExercise)

            else
                -- We are still on the same exercise part so we can return the value as-is
                Progress newProgressWithPossibleOverrun


goToPreviousExercise : ExerciseProgram -> Progress -> Progress
goToPreviousExercise ep progress =
    case progress of
        Progress p ->
            let
                previousExerciseId =
                    p.exerciseId - 1
            in
            if previousExerciseId < 0 then
                start

            else
                Progress
                    { p
                        | exercisePart = Prep
                        , timeMilliseconds = 0
                        , exerciseId = previousExerciseId
                    }

        Finished ->
            goToLastExercise ep


goToLastPartOfCurrentExercise : ExerciseProgram -> Progress -> Progress
goToLastPartOfCurrentExercise program progress =
    case currentExercise_ program progress of
        Nothing ->
            Finished

        Just ( exercise, p ) ->
            Progress
                { p
                    | exercisePart = Rep { index = exercise.repetitions - 1, isRest = False }
                }


goToLastExercise : ExerciseProgram -> Progress
goToLastExercise program =
    Progress
        { exercisePart = Prep
        , timeMilliseconds = 0
        , exerciseId = ExerciseProgram.length program - 1
        }


goToPreviousExerciseRep : ExerciseProgram -> Progress -> Progress
goToPreviousExerciseRep ep progress =
    case progress of
        Progress p ->
            case p.exercisePart of
                Prep ->
                    if p.exerciseId == 0 then
                        -- We are on the prep of the very first exercise. This is a special case where
                        -- we can just go to the start, there is no previous rep to go to
                        start

                    else
                        -- As we are on prep, we need to go to the end of the previous exercise
                        Progress p
                            |> goToPreviousExercise ep
                            |> goToLastPartOfCurrentExercise ep

                Rep rep ->
                    if rep.index == 0 then
                        -- Going back from the rep 0 exercise takes us to prep
                        Progress
                            { p
                                | exercisePart = Prep
                                , timeMilliseconds = 0
                            }

                    else
                        -- Goiing back from any other rep exercise simply decrements the rep
                        Progress
                            { p
                                | exercisePart = Rep { index = rep.index - 1, isRest = False }
                                , timeMilliseconds = 0
                            }

        Finished ->
            goToLastExercise ep
                |> goToLastPartOfCurrentExercise ep


goToNextExercise : ExerciseProgram -> Progress -> Progress
goToNextExercise ep progress =
    case progress of
        Progress p ->
            let
                nextExerciseId =
                    p.exerciseId + 1
            in
            case get nextExerciseId ep of
                Just _ ->
                    Progress
                        { p
                            | exercisePart = Prep
                            , timeMilliseconds = 0
                            , exerciseId = nextExerciseId
                        }

                Nothing ->
                    -- This is the case where there is no next exercise and
                    -- hence you have reached the finish. Also this handles
                    -- the edge case that the exerciseId was something weird like -2
                    Finished

        Finished ->
            --This is the case where you were already finished, so you are still
            --finished!
            Finished


goToNextExerciseRep : ExerciseProgram -> Progress -> Progress
goToNextExerciseRep program progress =
    case currentExercise_ program progress of
        Just ( exercise, p ) ->
            case p.exercisePart of
                Prep ->
                    Progress
                        { p
                            | exercisePart = Rep { index = 0, isRest = False }
                            , timeMilliseconds = 0
                        }

                Rep rep ->
                    if rep.index >= exercise.repetitions - 1 then
                        -- We are on the last rep so need to go forward to to next exercise
                        goToNextExercise program progress

                    else
                        Progress
                            { p
                                | exercisePart = Rep { index = rep.index + 1, isRest = False }
                                , timeMilliseconds = 0
                            }

        Nothing ->
            Finished



-- Private functions
--
-- Helpers used only in this file


calculateCurrentExercisePartTimeMilliseconds_ : ProgressInfo -> Exercise -> Int
calculateCurrentExercisePartTimeMilliseconds_ progress ex =
    case progress.exercisePart of
        Prep ->
            ex.prepDuration * 1000

        Rep r ->
            if r.isRest then
                ex.restDuration * 1000

            else
                ex.duration * 1000


{-| Helper for transition to next part of exercise when progressing through
time. Different to goToNextExerciseRep in that:

1.  This will move to rest and back, goToNextExerciseRep ignores rest
2.  This deals with ProgressInfo, so doesn't get concerned with Finished exercises

-}
goToNextPartOfExercise_ : Exercise -> ProgressInfo -> ProgressInfo
goToNextPartOfExercise_ ex progress =
    case progress.exercisePart of
        Prep ->
            { progress
                | exercisePart = Rep { index = 0, isRest = False }
                , timeMilliseconds = 0
            }

        Rep rep ->
            let
                nextRepIndex =
                    rep.index
                        + (if rep.isRest then
                            1

                           else
                            0
                          )
            in
            if nextRepIndex >= ex.repetitions then
                -- Exercises are now complete, move to next exercise set
                { exerciseId = progress.exerciseId + 1
                , exercisePart = Prep
                , timeMilliseconds = 0
                }

            else
                -- Still in progress, increment parameters accordingly
                { exerciseId = progress.exerciseId
                , exercisePart = Rep { index = nextRepIndex, isRest = not rep.isRest }
                , timeMilliseconds = 0
                }


currentExercise_ : ExerciseProgram -> Progress -> Maybe ( Exercise, ProgressInfo )
currentExercise_ program progress =
    case progress of
        Progress p ->
            ExerciseProgram.get p.exerciseId program |> Maybe.map (\ex -> ( ex, p ))

        Finished ->
            Nothing
