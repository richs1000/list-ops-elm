module RandomStuff exposing (..)


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


compressList : List a -> List a
compressList lst =
    let
        helperFunc oldLst newLst =
            case oldLst of
                [] ->
                    newLst

                o :: os ->
                    if (List.member o newLst) then
                        helperFunc os newLst
                    else
                        helperFunc os (o :: newLst)
    in
        helperFunc lst []


insertIntoList : a -> Int -> List a -> List a
insertIntoList item index lst =
    List.append (List.take index lst) (item :: (List.drop index lst))


randomizeListOrder : List Int -> List a -> List a
randomizeListOrder randomValues lst =
    let
        helperFunc rVals oldLst newLst =
            case oldLst of
                [] ->
                    newLst

                o :: [] ->
                    o :: newLst

                o :: os ->
                    let
                        lstLen =
                            List.length oldLst

                        index =
                            pickOne rVals [0..lstLen] 0

                        newLst' =
                            insertIntoList o index newLst
                    in
                        helperFunc (List.drop 1 rVals) os newLst'
    in
        helperFunc randomValues lst []
