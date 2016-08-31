module ModelType exposing (..)

import Question exposing (Question)
import HistoryTypes exposing (HistoryList)


-- MODEL


type alias Model =
    { debug : Bool
    , userInput : String
    , history : HistoryList
    , question : Question
    , success : Maybe Bool
    , feedback : String
    , randomValues : List Int
    , mastery : Bool
    , numerator : Int
    , denominator : Int
    , implementMastery : Bool
    }
