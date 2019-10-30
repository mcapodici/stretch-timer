module ExerciseProgram exposing (Exercise, ExerciseId, ExerciseProgram, add, decode, encode, get, isEmpty, length, new, remove, swap, toList, update)

import Array exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E



-- TODO make Exercise into an opaque, so that we can ensure non-negative (and not too big!) numbers and natural numbers where appropriate - thus
-- constructing an exercise requires a function that returns Maybe Exercise


type alias Exercise =
    { name : String
    , duration : Int
    , restDuration : Int
    , repetitions : Int
    , prepDuration : Int
    }


type alias ExerciseId =
    Int


type ExerciseProgram
    = EP (Array Exercise)



-- Exercise Progam Operations


new : ExerciseProgram
new =
    EP Array.empty


length : ExerciseProgram -> Int
length (EP prog) =
    Array.length prog


add : Exercise -> ExerciseProgram -> ExerciseProgram
add ex (EP prog) =
    EP <| Array.push ex prog


get : ExerciseId -> ExerciseProgram -> Maybe Exercise
get id (EP prog) =
    Array.get id prog


update : ExerciseId -> Exercise -> ExerciseProgram -> ExerciseProgram
update id ex (EP prog) =
    EP <| Array.set id ex prog


remove : ExerciseId -> ExerciseProgram -> ExerciseProgram
remove id (EP prog) =
    EP <| removeArrayItem id prog


swap : ExerciseId -> ExerciseId -> ExerciseProgram -> ExerciseProgram
swap id1 id2 (EP prog) =
    let
        mex1 =
            Array.get id1 prog

        mex2 =
            Array.get id2 prog
    in
    case ( mex1, mex2 ) of
        ( Just ex1, Just ex2 ) ->
            EP (prog |> Array.set id1 ex2 |> Array.set id2 ex1)

        _ ->
            EP prog


toList : ExerciseProgram -> List Exercise
toList (EP prog) =
    Array.toList prog


isEmpty : ExerciseProgram -> Bool
isEmpty (EP prog) =
    Array.length prog == 0


encode : ExerciseProgram -> E.Value
encode (EP prog) =
    E.array encodeExercise prog


decode : D.Decoder ExerciseProgram
decode =
    D.map (\arr -> EP arr) (D.array decodeExercise)


decodeExercise : D.Decoder Exercise
decodeExercise =
    D.succeed
        (\name duration restDuration repetitions prepDuration ->
            { name = name
            , duration = duration
            , restDuration = restDuration
            , repetitions = repetitions
            , prepDuration = prepDuration
            }
        )
        |> D.required "name" D.string
        -- Using optional a lot because I want the exercise to load with `something` even if
        -- some of the data is missing (e.g. from a previous version of the app). So when adding new fields will
        -- try to keep them optional, so that data isn't lost for users when upgrading.
        |> D.optional "duration" D.int 5
        |> D.optional "restDuration" D.int 5
        |> D.optional "repetitions" D.int 5
        |> D.optional "prepDuration" D.int 5



-- Private


encodeExercise : Exercise -> E.Value
encodeExercise ex =
    E.object
        [ ( "name", E.string ex.name )
        , ( "duration", E.int ex.duration )
        , ( "restDuration", E.int ex.restDuration )
        , ( "repetitions", E.int ex.repetitions )
        , ( "prepDuration", E.int ex.prepDuration )
        ]


removeArrayItem : Int -> Array a -> Array a
removeArrayItem i a =
    let
        a1 =
            Array.slice 0 i a

        a2 =
            Array.slice (i + 1) (Array.length a) a
    in
    Array.append a1 a2
