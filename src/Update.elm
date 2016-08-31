module Update exposing (..)

import Random exposing (..)
import Set exposing (..)
import String exposing (..)
import ModelType exposing (..)
import Question exposing (..)
import Ports exposing (..)
import Model exposing (..)
import MessageTypes exposing (Msg(..))
import Debug exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Startup flow: Reset -> NewRandomValues -> New Question Flow
        Reset ->
            ( initModel, Random.generate NewRandomValues (Random.list 15 (Random.int 1 15)) )

        NewRandomValues newValues ->
            ( { model | randomValues = newValues }
            , Random.generate NewQuestion (Random.int 1 3)
            )

        -- New Question Flow: NewQuestion -> UserInput -> Submit -> Give Feedback -> Check Mastery
        NewQuestion questionIndex ->
            let
                question' =
                    Debug.log "question index "
                        (newQuestion model.randomValues questionIndex)
            in
                ( { model | question = question', success = Nothing, userInput = "" }, Cmd.none )

        UserInput i ->
            ( { model | userInput = i }, Cmd.none )

        Submit ->
            if (String.isEmpty model.userInput) then
                ( model, Cmd.none )
            else
                -- ( checkAnswer model, Cmd.none )
                let
                    newHistory =
                        List.take (model.denominator - 1) model.history

                    { question, distractors, answer } =
                        model.question
                in
                    if (fst answer) == model.userInput then
                        ( { model | success = Just True, history = (Just True) :: newHistory, feedback = (snd answer) }, Cmd.none )
                    else
                        ( { model | success = Just False, history = (Just False) :: newHistory, feedback = (findFeedback (fst answer) model.userInput distractors) }, Cmd.none )

        GiveFeedback ->
            update CheckMastery model

        CheckMastery ->
            if masteryAchieved model then
                update UpdateMastery { model | mastery = True }
            else
                ( { model | mastery = False }
                , Random.generate NewRandomValues (Random.list 15 (Random.int 1 15))
                )

        UpdateMastery ->
            ( model, updateMastery model.mastery )

        GetValuesFromSS ssd ->
            let
                debug' =
                    Debug.log "got values from smart sparrow: "
                        ssd.debug
            in
                ( { model
                    | mastery = ssd.mastery
                    , numerator = ssd.numerator
                    , denominator = ssd.denominator
                    , implementMastery = ssd.implementMastery
                    , debug = ssd.debug
                  }
                , Random.generate NewRandomValues (Random.list 15 (Random.int 1 15))
                )
