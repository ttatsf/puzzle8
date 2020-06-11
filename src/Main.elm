module Main exposing (main)

-- import Array exposing (Array)

import Arithmetic exposing (isEven)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import List.Extra
import Maybe
import Random
import Random.List



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { tiles : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model solved
    , Random.generate NewOrder <| Random.List.shuffle solved
    )


solved =
    [ 1, 2, 3, 4, 5, 6, 7, 8, spaceValue ]


spaceValue =
    9


defaultSpaceIndex =
    8


matrixSize =
    3



-- UPDATE


type Msg
    = Reset
    | NewOrder (List Int)
    | Click Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( model
            , Random.generate NewOrder <| Random.List.shuffle solved
            )

        NewOrder newOrder ->
            ( Model  <| solvabled <| newOrder
            , Cmd.none
            )

        Click value ->
            ( { model | tiles = model.tiles |> moveBy value }
            , Cmd.none
            )


solvabled : List Int -> List Int
solvabled list =
    if isSolvable list then
        list

    else if List.member (indexOf spaceValue list) [ 0, 1 ] then
        swap 2 3 list

    else
        swap 0 1 list


swap : Int -> Int -> List Int -> List Int
swap a b list =
    List.Extra.swapAt a b list


isSolvable : List Int -> Bool
isSolvable list =
    exchangingParity list == spaceMovementParity list


indexOf : Int -> List Int -> Int
indexOf value list =
    Maybe.withDefault -1 <| List.Extra.elemIndex value list


type Parity
    = Odd
    | Even


inverted : Parity -> Parity
inverted parity =
    if parity == Odd then
        Even

    else
        Odd


type alias Temporary =
    { list : List Int
    , parity : Parity
    }


exchangingParity : List Int -> Parity
exchangingParity list =
    list 
        |> List.indexedMap Tuple.pair 
        |> List.foldl updateEP (Temporary list Even)
        |> .parity

updateEP : ( Int, Int ) -> Temporary -> Temporary
updateEP ( i, _ ) temp =
    if valueOf i temp.list == valueOf i solved then
        temp

    else
        { temp
            | list = swap i (indexOf (valueOf i solved) temp.list) temp.list
            , parity = inverted temp.parity
        }


valueOf : Int -> List Int -> Int
valueOf i list =
    Maybe.withDefault -1 <| List.Extra.getAt i list


spaceMovementParity : List Int -> Parity
spaceMovementParity list =
    if Arithmetic.isEven <| distance (indexOf spaceValue list) defaultSpaceIndex then
        Even

    else
        Odd


moveBy : Int -> List Int -> List Int
moveBy value list =
    let
        valueIndex =
            indexOf value list

        spaceIndex =
            indexOf spaceValue list
    in
    if distance valueIndex spaceIndex == 1 then
        list |> swap valueIndex spaceIndex

    else
        list


distance : Int -> Int -> Int
distance a b =
    abs (modBy matrixSize a - modBy matrixSize b) + abs (a // matrixSize - b // matrixSize)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ div [ class "panel" ]
            (List.map putTile model.tiles)
        , div [ class "reset", onClick Reset ] [ text "Reset" ]
        ]


putTile : Int -> Html Msg
putTile value =
    if value == spaceValue then
        div
            [ class "tile tile_no_space"
            , onClick (Click value)
            ]
            [ text "_" ]

    else
        div
            [ class ("tile tile_no_" ++ String.fromInt value)
            , onClick (Click value)
            ]
            [ text (String.fromInt value) ]

