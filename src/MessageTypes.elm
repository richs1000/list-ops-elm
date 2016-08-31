module MessageTypes exposing (..)


type alias SSData =
    { mastery : Bool
    , numerator : Int
    , denominator : Int
    , implementMastery : Bool
    , debug : Bool
    }


type Msg
    = Reset
    | NewRandomValues (List Int)
    | NewQuestion Int
    | UserInput String
    | Submit
    | GiveFeedback
    | CheckMastery
    | UpdateMastery
    | GetValuesFromSS SSData
