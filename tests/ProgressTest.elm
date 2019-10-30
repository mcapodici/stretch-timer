module ProgressTest exposing (suite)

import ExerciseProgram exposing (ExerciseProgram)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Progress exposing (ExercisePart(..), Progress(..))
import Test exposing (..)


exampleExerciseProgram : ExerciseProgram
exampleExerciseProgram =
    ExerciseProgram.new
        |> ExerciseProgram.add
            { name = "Triceps"
            , duration = 30
            , restDuration = 15
            , repetitions = 4
            , prepDuration = 10
            }
        |> ExerciseProgram.add
            { name = "Biceps"
            , duration = 30
            , restDuration = 15
            , repetitions = 4
            , prepDuration = 10
            }


suite : Test
suite =
    describe "Progress"
        [ describe "start"
            [ test "start has correct info"
                (\_ ->
                    Expect.equal
                        (Progress
                            { exerciseId = 0
                            , exercisePart = Prep
                            , timeMilliseconds = 0
                            }
                        )
                        Progress.start
                )
            ]
        , describe "incrementByMilliseconds"
            (let
                addNTimes progress ms steps =
                    if steps <= 0 then
                        progress

                    else
                        let
                            nextStep =
                                Progress.incrementByMilliseconds exampleExerciseProgram ms progress
                        in
                        if steps == 1 then
                            nextStep

                        else
                            addNTimes nextStep ms (steps - 1)

                fromStart ms steps =
                    addNTimes Progress.start ms steps

                assert ms steps expected =
                    Expect.equal (Progress expected) (fromStart ms steps)
             in
             [ test "incrementing within same exercise part"
                (\_ ->
                    assert 11000
                        1
                        { exerciseId = 0
                        , exercisePart = Rep { index = 0, isRest = False }
                        , timeMilliseconds = 1000
                        }
                )
             , test "incrementing to rest part at zero"
                (\_ ->
                    assert 40000
                        1
                        { exerciseId = 0
                        , exercisePart = Rep { index = 0, isRest = True }
                        , timeMilliseconds = 0
                        }
                )
             , test "incrementing to rest part at 1"
                (\_ ->
                    assert 40001
                        1
                        { exerciseId = 0
                        , exercisePart = Rep { index = 0, isRest = True }
                        , timeMilliseconds = 1
                        }
                )
             , test "incrementing to next rep at 0"
                (\_ ->
                    assert 55000
                        1
                        { exerciseId = 0
                        , exercisePart = Rep { index = 1, isRest = False }
                        , timeMilliseconds = 0
                        }
                )
             , test "incrementing to next rep at 1"
                (\_ ->
                    assert 55001
                        1
                        { exerciseId = 0
                        , exercisePart = Rep { index = 1, isRest = False }
                        , timeMilliseconds = 1
                        }
                )
             , test "incrementing to next rep and rest at 0"
                (\_ ->
                    assert 85000
                        1
                        { exerciseId = 0
                        , exercisePart = Rep { index = 1, isRest = True }
                        , timeMilliseconds = 0
                        }
                )
             , test "incrementing to next rep and rest at 1"
                (\_ ->
                    assert 85001
                        1
                        { exerciseId = 0
                        , exercisePart = Rep { index = 1, isRest = True }
                        , timeMilliseconds = 1
                        }
                )
             , test "incrementing to next set at 0"
                (\_ ->
                    assert 200000
                        1
                        { exerciseId = 1
                        , exercisePart = Rep { index = 0, isRest = False }
                        , timeMilliseconds = 0
                        }
                )
             , test "incrementing to next set at 1"
                (\_ ->
                    assert 200001
                        1
                        { exerciseId = 1
                        , exercisePart = Rep { index = 0, isRest = False }
                        , timeMilliseconds = 1
                        }
                )
             , test "incrementing to just before"
                (\_ ->
                    assert 379999
                        1
                        { exerciseId = 1
                        , exercisePart = Rep { index = 3, isRest = True }
                        , timeMilliseconds = 14999
                        }
                )
             , test "incrementing to finish"
                (\_ ->
                    Expect.equal Finished (fromStart 380000 1)
                )
             , test "multistep - incrementing within prep"
                (\_ ->
                    assert 1000
                        2
                        { exerciseId = 0
                        , exercisePart = Prep
                        , timeMilliseconds = 2000
                        }
                )
             , test "multistep - incrementing to rest part at zero"
                (\_ ->
                    assert 1000
                        40
                        { exerciseId = 0
                        , exercisePart = Rep { index = 0, isRest = True }
                        , timeMilliseconds = 0
                        }
                )
             , test "multistep - incrementing to next rep at 0"
                (\_ ->
                    assert 1000
                        55
                        { exerciseId = 0
                        , exercisePart = Rep { index = 1, isRest = False }
                        , timeMilliseconds = 0
                        }
                )
             , test "multistep - incrementing to next rep and rest at 0"
                (\_ ->
                    assert 1000
                        85
                        { exerciseId = 0
                        , exercisePart = Rep { index = 1, isRest = True }
                        , timeMilliseconds = 0
                        }
                )
             , test "multistep - incrementing to next set at the prep stage"
                (\_ ->
                    assert 1000
                        190
                        { exerciseId = 1
                        , exercisePart = Prep
                        , timeMilliseconds = 0
                        }
                )
             , test "multistep - incrementing to next set at the 0"
                (\_ ->
                    assert 1000
                        200
                        { exerciseId = 1
                        , exercisePart = Rep { index = 0, isRest = False }
                        , timeMilliseconds = 0
                        }
                )
             , test "multistep - incrementing to finish"
                (\_ ->
                    Expect.equal Finished (fromStart 1000 380)
                )
             ]
            )
        , describe "remainingPartTimeMilliseconds"
            [ test "1"
                (\_ ->
                    Expect.equal 29000
                        (Progress.remainingPartTimeMilliseconds exampleExerciseProgram
                            (Progress
                                { exerciseId = 0
                                , exercisePart = Rep { index = 1, isRest = False }
                                , timeMilliseconds = 1000
                                }
                            )
                        )
                )
            ]
        , describe "remainingPartTimePercent"
            [ test "1"
                (\_ ->
                    Expect.within (Absolute 0.0001)
                        96.66666666666667
                        (Progress.remainingPartTimePercent exampleExerciseProgram
                            (Progress
                                { exerciseId = 0
                                , exercisePart = Rep { index = 1, isRest = False }
                                , timeMilliseconds = 1000
                                }
                            )
                        )
                )
            ]
        , describe "goToPreviousExerciseRep"
            (let
                assert before after =
                    Expect.equal (Progress after) (Progress.goToPreviousExerciseRep exampleExerciseProgram (Progress before))
             in
             [ test "previous rep is in the current exercise"
                (\_ ->
                    assert
                        { exerciseId = 0
                        , exercisePart = Rep { index = 1, isRest = False }
                        , timeMilliseconds = 1000
                        }
                        { exerciseId = 0
                        , exercisePart = Rep { index = 0, isRest = False }
                        , timeMilliseconds = 0
                        }
                )
             , test "previous rep is in the current exercise, but was resting"
                (\_ ->
                    assert
                        { exerciseId = 0
                        , exercisePart = Rep { index = 1, isRest = True }
                        , timeMilliseconds = 1000
                        }
                        { exerciseId = 0
                        , exercisePart = Rep { index = 0, isRest = False }
                        , timeMilliseconds = 0
                        }
                )
             , test "previous rep takes us from ex 0 to prep"
                (\_ ->
                    assert
                        { exerciseId = 0
                        , exercisePart = Rep { index = 0, isRest = False }
                        , timeMilliseconds = 1000
                        }
                        { exerciseId = 0
                        , exercisePart = Prep
                        , timeMilliseconds = 0
                        }
                )
             , test "previous rep takes us to previous exercise"
                (\_ ->
                    assert
                        { exerciseId = 1
                        , exercisePart = Prep
                        , timeMilliseconds = 1000
                        }
                        { exerciseId = 0
                        , exercisePart = Rep { index = 3, isRest = False }
                        , timeMilliseconds = 0
                        }
                )
             , test "start stays at start"
                (\_ ->
                    assert
                        { exerciseId = 0
                        , exercisePart = Prep
                        , timeMilliseconds = 0
                        }
                        { exerciseId = 0
                        , exercisePart = Prep
                        , timeMilliseconds = 0
                        }
                )
             , test "prep in progress goes to start"
                (\_ ->
                    assert
                        { exerciseId = 0
                        , exercisePart = Prep
                        , timeMilliseconds = 1000
                        }
                        { exerciseId = 0
                        , exercisePart = Prep
                        , timeMilliseconds = 0
                        }
                )
             ]
            )
        , describe "goToNextExerciseRep"
            (let
                assert before after =
                    Expect.equal after (Progress.goToNextExerciseRep exampleExerciseProgram (Progress before))
             in
             [ test "next rep is in the current exercise"
                (\_ ->
                    assert
                        { exerciseId = 0
                        , exercisePart = Rep { index = 1, isRest = False }
                        , timeMilliseconds = 1000
                        }
                        (Progress
                            { exerciseId = 0
                            , exercisePart = Rep { index = 2, isRest = False }
                            , timeMilliseconds = 0
                            }
                        )
                )
             , test "next rep is in the current exercise, but was resting"
                (\_ ->
                    assert
                        { exerciseId = 0
                        , exercisePart = Rep { index = 1, isRest = True }
                        , timeMilliseconds = 1000
                        }
                        (Progress
                            { exerciseId = 0
                            , exercisePart = Rep { index = 2, isRest = False }
                            , timeMilliseconds = 0
                            }
                        )
                )
             , test "next rep takes us to end"
                (\_ ->
                    assert
                        { exerciseId = 1
                        , exercisePart = Rep { index = 3, isRest = False }
                        , timeMilliseconds = 1000
                        }
                        Finished
                )
             , test "next rep takes us to next exercise"
                (\_ ->
                    assert
                        { exerciseId = 0
                        , exercisePart = Rep { index = 3, isRest = False }
                        , timeMilliseconds = 1000
                        }
                        (Progress
                            { exerciseId = 1
                            , exercisePart = Prep
                            , timeMilliseconds = 0
                            }
                        )
                )
             ]
            )
        ]
