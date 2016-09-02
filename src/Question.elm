module Question exposing (..)

import RandomStuff exposing (..)


type QuestionFormat
    = FillInTheBlank
    | MultipleChoice


type alias Question =
    { question : List String
    , distractors : List ResponseAndFeedback
    , answer : ResponseAndFeedback
    , format : QuestionFormat
    }


type alias ResponseAndFeedback =
    ( String, String )


emptyQuestion : Question
emptyQuestion =
    { question = []
    , distractors = []
    , answer = ( "", "" )
    , format = FillInTheBlank
    }


newQuestion : List Int -> Int -> Question
newQuestion randomValues index =
    let
        subListOne =
            pickABunch (List.drop 1 randomValues) 3 [0..9] (pickOne randomValues [ 1, 2, 3 ] 1)
    in
        -- list is one deep, head or tail
        if index == 1 || index == 2 then
            let
                myOp =
                    pickOne (List.drop 10 randomValues) [ "hd e", "tl e", "tl (tl e)", "tl (tl (tl e))" ] "hd e"

                question' =
                    [ "What is the value of ans after the following ML expressions are evaluated?"
                    , "e = " ++ toString (subListOne)
                    , "ans = " ++ myOp
                    ]

                distractors =
                    [ ( "hd e", toString (Maybe.withDefault 0 (List.head subListOne)) )
                    , ( "tl e", toString (List.drop 1 subListOne) )
                    , ( "tl (tl e)", toString (List.drop 2 subListOne) )
                    , ( "tl (tl (tl e))", toString (List.drop 3 subListOne) )
                    ]

                ( answers, distractors' ) =
                    List.partition (\( op, _ ) -> op == myOp) distractors

                answer' =
                    Maybe.withDefault ( "uh oh", "uh oh" ) (List.head answers)

                format' =
                    if index == 1 then
                        MultipleChoice
                    else
                        FillInTheBlank
            in
                { question = question'
                , distractors = List.map (\( _, dis ) -> ( dis, "Incorrect." )) distractors'
                , answer = ( snd answer', "Correct" )
                , format = format'
                }
            -- list is one deep, append
        else
            let
                newItem =
                    pickOne (List.drop 10 randomValues) [0..9] 0

                question' =
                    [ "What is the value of ans after the following ML expressions are evaluated?"
                    , "e = " ++ toString (subListOne)
                    , "ans = " ++ toString newItem ++ " :: e"
                    ]

                distractors' =
                    [ ( toString newItem, "Incorrect. The :: operation adds the item on the left to the front of the list on the right. " )
                    , ( toString (List.append subListOne [ newItem ]), "Incorrect. The :: operation adds the item on the left to the front of the list on the right. " )
                    , ( toString (newItem :: (List.drop 1 subListOne)), "Incorrect. The :: operation adds the item on the left to the front of the list on the right" )
                    ]

                answer' =
                    ( toString (newItem :: subListOne)
                    , "Correct. The :: operation adds the item on the left to the front of the list on the right. "
                    )

                format' =
                    if index == 3 then
                        MultipleChoice
                    else
                        FillInTheBlank
            in
                { question = question'
                , distractors = distractors'
                , answer = ( fst answer', "Correct" )
                , format = format'
                }


findFeedback : String -> String -> List ResponseAndFeedback -> String
findFeedback answer response distractors =
    case distractors of
        [] ->
            "Incorrect. The answer is " ++ answer

        d :: ds ->
            if ((fst d) == response || ((fst d) == "")) then
                (snd d) ++ " The answer is " ++ answer
            else
                findFeedback answer response ds
