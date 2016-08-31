module Question exposing (..)


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


pickOne : List Int -> List a -> a -> a
pickOne randomValues lst defVal =
    let
        -- get a random value
        rv =
            Maybe.withDefault 0 (List.head randomValues)

        -- use that to choose a random index within the list
        index =
            rv `rem` (List.length lst)
    in
        -- extract the list item at the index position
        lst
            |> List.drop index
            |> List.head
            |> Maybe.withDefault defVal


pickABunch : List Int -> Int -> List a -> a -> List a
pickABunch randomValues cnt lst defVal =
    if cnt == 0 then
        []
    else
        (pickOne randomValues lst defVal) :: (pickABunch (List.drop 1 randomValues) (cnt - 1) lst defVal)


newQuestion : List Int -> Int -> Question
newQuestion randomValues index =
    let
        subListOne =
            pickABunch (List.drop 1 randomValues) 3 [0..9] (pickOne randomValues [ 1, 2, 3 ] 1)

        subListTwo =
            pickABunch (List.drop 5 randomValues) 3 [0..9] (pickOne (List.drop 4 randomValues) [ 1, 2, 3 ] 1)

        subListThree =
            pickABunch (List.drop 9 randomValues) 3 [0..9] (pickOne (List.drop 8 randomValues) [ 1, 2, 3 ] 1)
    in
        -- list is one deep, head or tail
        if index == 1 then
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
            in
                { question = question'
                , distractors = List.map (\( _, dis ) -> ( dis, "Incorrect." )) distractors'
                , answer = ( snd answer', "Correct" )
                , format = MultipleChoice
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
            in
                { question = question'
                , distractors = distractors'
                , answer = ( fst answer', "Correct" )
                , format = MultipleChoice
                }



--         myOp =
--             pickOne (List.drop 1 randomValues) [ ">", ">=", "<", "<=" ] ">"
--
--         myOp' =
--             if myOp == ">" then
--                 "greater than"
--             else if myOp == ">=" then
--                 "greater than or equal to"
--             else if myOp == "<=" then
--                 "less than or equal to"
--             else
--                 "less than"
--
--         question' =
--             [ "Given this ML expression:"
--             , "val e1 = e2 " ++ myOp ++ " e3"
--             , "What is the type of " ++ myVar ++ "?"
--             ]
--
--         distractors' =
--             if myVar == "e1" then
--                 [ ( "string"
--                   , "Incorrect. " ++ myVar ++ " is the result of a " ++ myOp' ++ " expression, so it must be a bool"
--                   )
--                 , ( "int"
--                   , "Incorrect. " ++ myVar ++ " is the result of a " ++ myOp' ++ " expression, so it must be a bool"
--                   )
--                 , ( "It can have any type"
--                   , "Incorrect. " ++ myVar ++ " is the result of a " ++ myOp' ++ " expression, so it must be a bool"
--                   )
--                 ]
--             else
--                 [ ( "string"
--                   , "Incorrect. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
--                   )
--                 , ( "bool"
--                   , "Incorrect. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
--                   )
--                 , ( "It can have any type"
--                   , "Incorrect. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
--                   )
--                 ]
--
--         answer' =
--             if myVar == "e1" then
--                 ( "bool"
--                 , "Correct. " ++ myVar ++ " is the result of a " ++ myOp' ++ " expression, so it must be a bool"
--                 )
--             else
--                 ( "int"
--                 , "Correct. " ++ myVar ++ " is part of a " ++ myOp' ++ " expression, so it must be an int"
--                 )
--     in
--         { question = question'
--         , distractors = distractors'
--         , answer = answer'
--         , format = MultipleChoice
--         }
-- else if index == 3 && myVar == "e1" then
--     let
--         question' =
--             [ "Given this ML expression:"
--             , "if e1 then e2 else e3"
--             , "What is the type of " ++ myVar ++ "?"
--             ]
--
--         distractors' =
--             [ ( "string"
--               , "Incorrect. e1 is the test condition within an if-then-else expression, so it must be a bool"
--               )
--             , ( "int"
--               , "Incorrect. e1 is the test condition within an if-then-else expression, so it must be a bool"
--               )
--             , ( "It can have any type"
--               , "Incorrect. e1 is the test condition within an if-then-else expression, so it must be a bool"
--               )
--             ]
--
--         answer' =
--             ( "bool"
--             , "Correct. e1 is the test condition within an if-then-else expression, so it must be a bool"
--             )
--     in
--         { question = question'
--         , distractors = distractors'
--         , answer = answer'
--         , format = MultipleChoice
--         }
-- else
--     let
--         -- if the first variable in the question is e2, the second is e3 (and vice versa)
--         myVar2 =
--             if myVar == "e2" then
--                 "e3"
--             else
--                 "e2"
--
--         -- pick a type randomly
--         myAnswer =
--             pickOne (List.drop 1 randomValues) [ "int", "bool", "string" ] "int"
--
--         -- get the remaining types, which are distractors
--         ( _, myDistractors ) =
--             List.partition (\s -> s == myAnswer) [ "int", "bool", "string", "It can have any type" ]
--
--         question' =
--             [ "Given this ML expression:"
--             , "if e1 then e2 else e3"
--             , "If " ++ myVar ++ " has type " ++ myAnswer ++ ", what is the type of " ++ myVar2 ++ "?"
--             ]
--
--         answer' =
--             ( myAnswer
--             , "Correct. Both branches of an if-then-else statement must have the same type."
--             )
--
--         distractors' =
--             List.foldr
--                 (\d ds -> ( d, "Incorrect. Both branches of an if-then-else statement must have the same type." ) :: ds)
--                 []
--                 myDistractors
--     in
--         { question = question'
--         , distractors = distractors'
--         , answer = answer'
--         , format = MultipleChoice
--         }


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
