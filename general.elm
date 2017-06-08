module General exposing (zip, zipFill)

{-| The zip function takes in two lists and returns a combined
list. It combines the elements of each list pairwise until one
of the lists runs out of elements.

    zip [1,2,3] ['a','b','c'] == [(1,'a'), (2,'b'), (3,'c')]

-}


zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( x :: xBack, y :: yBack ) ->
            ( x, y ) :: zip xBack yBack

        ( _, _ ) ->
            []


zipFill : (() -> a) -> List a -> List a -> List ( a, a )
zipFill filler xs ys =
    -- this could probably be smarter
    let
        zip =
            zipFill filler
    in
        case ( xs, ys ) of
            ( x :: xtail, y :: ytail ) ->
                ( x, y ) :: zip xtail ytail

            ( [], y :: ytail ) ->
                ( filler (), y ) :: zip [] ytail

            ( x :: xtail, [] ) ->
                ( x, filler () ) :: zip xtail []

            ( _, _ ) ->
                []
