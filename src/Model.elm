module Model exposing (..)

import ModelType exposing (Model)
import MessageTypes exposing (Msg(..))
import Question exposing (emptyQuestion)


masteryAchieved : Model -> Bool
masteryAchieved model =
    let
        correctAnswers =
            Debug.log "in masteryAchieved "
                List.foldr
                (\h acc ->
                    if h == Just True then
                        acc + 1
                    else
                        acc
                )
                0
                model.history
    in
        if model.implementMastery then
            correctAnswers >= model.numerator
        else
            (List.length model.history) >= model.numerator


initModel : Model
initModel =
    { debug = True
    , userInput = ""
    , history = []
    , success = Nothing
    , question = emptyQuestion
    , feedback = ""
    , randomValues = []
    , mastery = False
    , numerator = 3
    , denominator = 5
    , implementMastery = False
    }


init : ( Model, Cmd Msg )
init =
    -- update Reset initModel
    ( initModel, Cmd.none )
