module Main exposing (..)

import ExerciseRunner exposing (..)
import Html exposing (Html)
import Html.Attributes
import String exposing (..)
import List exposing (..)
import Tuple exposing (..)


--
-- Strings
--


sayHello : String -> String
sayHello friendsName =
    "Hello, " ++ friendsName


formatPhoneNumber : String -> String -> String -> String
formatPhoneNumber areaCode exchange local =
    -- desired format: (999) 999-9999
    "(" ++ areaCode ++ ")" ++ " " ++ exchange ++ "-" ++ local


firstLetter : String -> String
firstLetter word =
    left 1 word


initials : String -> String -> String
initials firstName lastName =
    -- HINT: look at http://package.elm-lang.org/packages/elm-lang/core/latest/String for useful functions
    (left 1 firstName) ++ (left 1 lastName)


stringUpper : String -> String
stringUpper =
    String.left 1 >> String.toUpper


stringLower : String -> String
stringLower =
    String.left 1 >> String.toLower


pigLatin : String -> String
pigLatin word =
    -- To go from English to pig latin, take the first letter of the word
    -- and put it at the end of the word, followed by an "ay" sound.
    -- Pig latin, in pig latin, is "Ig-pay atin-lay"
    -- For this exercise, just worry about producing a single word in pig latin.
    let
        splitWord =
            case String.uncons word of
                Nothing ->
                    ( ' ', "" )

                Just ( char, str ) ->
                    ( char, str )

        firstPart =
            (toLower (fromChar (first splitWord))) ++ "ay"

        secondPart =
            case String.uncons (second splitWord) of
                Nothing ->
                    ""

                Just ( char, str ) ->
                    (toUpper (fromChar char)) ++ str
    in
        secondPart ++ "-" ++ firstPart



--
-- If Statements
--


isGreaterThanTen : Int -> Bool
isGreaterThanTen x =
    -- TODO: implement me
    if x > 10 then
        True
    else
        False


howHotIsThePepper : Float -> String
howHotIsThePepper heatUnits =
    if heatUnits == 2.0 then
        "not hot"
    else if heatUnits == 100.0 then
        "mild"
    else if heatUnits == 3000.0 then
        "medium"
    else
        "hot"



--
-- Lists
--


reverseTheList : List a -> List a
reverseTheList inputList =
    List.reverse inputList


increment : Int -> Int
increment num =
    num + 1


addOne : List Int -> List Int
addOne inputList =
    List.map increment inputList


checkForO : String -> Bool
checkForO string =
    (not (contains "O" string))


removeOs : List String -> List String
removeOs inputList =
    -- TODO: remove all entries that start with "O"
    List.filter checkForO inputList



--
-- Records
--


type alias Person =
    { name : String, age : Int }


newborn : String -> Person
newborn name =
    { name = name, age = 0 }


ageDifference : Person -> Person -> Int
ageDifference person1 person2 =
    abs (person1.age - person2.age)


nameChange : String -> Person -> Person
nameChange newName person =
    { name = newName, age = person.age }


getOlder : Person -> Person
getOlder person =
    { name = person.name, age = (increment person.age) }


addAge : Person -> Int -> Int
addAge person num =
    num + person.age


combinedYears : List Person -> Int
combinedYears people =
    List.foldr (addAge) 0 people



--
-- Tuples
--


signAndMagnitude : Int -> ( String, Int )
signAndMagnitude x =
    -- TODO: implement me
    let
        getSign =
            case String.uncons (toString x) of
                Nothing ->
                    ""

                Just ( char, string ) ->
                    (fromChar char)
    in
        case getSign of
            "-" ->
                ( "-", (abs x) )

            _ ->
                ( "+", x )



--
-- Congratulations!  You've finished!
--
--
--
--
-- There's no need to look below this line (unless you are curious)
--


examples : List ( String, List Example )
examples =
    [ ( "Strings"
      , [ functionExample1 "sayHello"
            sayHello
            [ ( "Jasmine", "Hello, Jasmine" )
            , ( "Jean", "Hello, Jean" )
            ]
        , functionExample3 "formatPhoneNumber"
            formatPhoneNumber
            [ ( ( "347", "489", "4608" ), "(347) 489-4608" )
            , ( ( "800", "555", "2368" ), "(800) 555-2368" )
            ]
        , functionExample2 "initials"
            initials
            [ ( ( "Ada", "Yonath" ), "AY" )
            , ( ( "Kimberlé", "Crenshaw" ), "KC" )
            , ( ( "Dorothy", "Hodgkin" ), "DH" )
            ]
        , functionExample1 "pigLatin"
            pigLatin
            [ ( "Pig", "Ig-pay" )
            , ( "Latin", "Atin-lay" )
            ]
        ]
      )
    , ( "If Statements"
      , [ functionExample1 "isGreaterThanTen"
            isGreaterThanTen
            [ ( 13, True )
            , ( 3, False )
            , ( 10, False )
            ]
        , functionExample1 "howHotIsThePepper"
            howHotIsThePepper
            [ ( 2, "not hot" )
            , ( 100, "mild" )
            , ( 3000, "medium" )
            , ( 50000, "hot" )
            ]
        ]
      )
    , ( "Lists"
      , [ functionExample1 "reverseTheList"
            reverseTheList
            [ ( [ 7, 0, 1, 4, 9 ], [ 9, 4, 1, 0, 7 ] )
            , ( [ 99, -1 ], [ -1, 99 ] )
            ]
        , functionExample1 "addOne"
            addOne
            [ ( [ 7, 0, 1, 4, 9 ], [ 8, 1, 2, 5, 10 ] )
            , ( [ 99, -1 ], [ 100, 0 ] )
            ]
        , functionExample1 "removeOs"
            removeOs
            [ ( [ "Jessie", "Anibus", "Osirus" ], [ "Jessie", "Anibus" ] )
            , ( [ "Apple", "Banana" ], [ "Apple", "Banana" ] )
            , ( [ "Octothorpe", "Octohash" ], [] )
            ]
        ]
      )
    , ( "Records"
      , [ functionExample1 "newborn"
            newborn
            [ ( "Jenny", { name = "Jenny", age = 0 } )
            , ( "Abey", { name = "Abey", age = 0 } )
            ]
        , functionExample2 "ageDifference"
            ageDifference
            [ ( ( { name = "Nicole", age = 40 }, { name = "Angel", age = 30 } ), 10 )
            , ( ( { name = "Igor", age = 18 }, { name = "Alexei", age = 23 } ), 5 )
            ]
        , functionExample2 "nameChange"
            nameChange
            [ ( ( "Mr. T", { name = "Laurence", age = 34 } ), { name = "Mr. T", age = 34 } )
            , ( ( "Demi", { name = "Demetria", age = 17 } ), { name = "Demi", age = 17 } )
            , ( ( "Ƭ̵̬̊", { name = "Prince", age = 35 } ), { name = "Ƭ̵̬̊", age = 35 } )
            ]
        , functionExample1 "getOlder"
            getOlder
            [ ( { name = "Jenny", age = 0 }, { name = "Jenny", age = 1 } )
            , ( { name = "Igor", age = 18 }, { name = "Igor", age = 19 } )
            ]
        , functionExample1 "combinedYears"
            combinedYears
            [ ( [ { name = "Ruth Bader Ginsburg", age = 83 }
                , { name = "Gloria Allred", age = 75 }
                , { name = "Caroline Kennedy", age = 58 }
                ]
              , 216
              )
            , ( [ { name = "Amy Poehler", age = 45 }
                , { name = "Chris Pratt", age = 37 }
                , { name = "Rashida Jones", age = 40 }
                , { name = "Aziz Ansari", age = 33 }
                , { name = "Retta", age = 46 }
                ]
              , 201
              )
            ]
        ]
      )

    -- , ( "HTML", [] )
    ]


bonusExamples : List ( String, List Example )
bonusExamples =
    [ ( "Tuples"
      , [ functionExample1 "signAndMagnitude"
            signAndMagnitude
            [ ( -7, ( "-", 7 ) )
            , ( 3, ( "+", 3 ) )
            , ( 10, ( "+", 10 ) )
            , ( -44, ( "-", 44 ) )
            ]
        ]
      )

    -- , ( "Union types", [] )
    -- , ( "Case statements", [] )
    -- , ( "Maybes", [] )
    -- , ( "Complex case statements", [] )
    -- , ( "Dictionaries", [] )
    ]


main : Html Never
main =
    Html.div
        [ Html.Attributes.style [ ( "padding", "20px" ) ] ]
        [ fontStyles
        , examples
            |> List.map (\( title, x ) -> viewExampleSection title x)
            |> Html.div []
        , Html.h1 [] [ Html.text "Bonus Sections" ]
        , bonusExamples
            |> List.map (\( title, x ) -> viewExampleSection title x)
            |> Html.div []
        ]
