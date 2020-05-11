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
    ( { tiles = solved }
    , Cmd.none
    )


solved =
    [ 1, 2, 3, 4, 5, 6, 7, 8, space ]


space =
    9


defaultSpace =
    8


size =
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
            , Random.generate NewOrder (Random.List.shuffle solved)
            )

        NewOrder newOrder ->
            ( Model (newOrder |> solvabled)
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

    else if List.member (indexOf space list) [ 0, 1 ] then
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
    Maybe.withDefault -1 (List.Extra.elemIndex value list)


type Parity
    = Odd
    | Even


inverted : Parity -> Parity
inverted parity =
    if parity == Odd then
        Even

    else
        Odd


type alias Progressing =
    { list : List Int
    , parity : Parity
    }


exchangingParity : List Int -> Parity
exchangingParity list =
    List.foldl updateEP (Progressing list Even) (List.indexedMap Tuple.pair list)
        |> .parity


updateEP : ( Int, Int ) -> Progressing -> Progressing
updateEP ( i, _ ) progressing =
    if index i progressing.list == index i solved then
        progressing

    else
        { progressing
            | list = swap i (indexOf (index i solved) progressing.list) progressing.list
            , parity = inverted progressing.parity
        }


index : Int -> List Int -> Int
index i list =
    Maybe.withDefault -1 (List.Extra.getAt i list)


spaceMovementParity : List Int -> Parity
spaceMovementParity list =
    if Arithmetic.isEven (distance (indexOf space list) defaultSpace) then
        Even

    else
        Odd


moveBy : Int -> List Int -> List Int
moveBy value list =
    let
        positionValue =
            indexOf value list

        positionSpace =
            indexOf space list
    in
    if distance positionValue positionSpace == 1 then
        list |> swap positionValue positionSpace

    else
        list


distance : Int -> Int -> Int
distance a b =
    abs (modBy size a - modBy size b) + abs (a // size - b // size)



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
    div [ class "panel__tile-wrapper" ]
        [ if value == space then
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
        ]
