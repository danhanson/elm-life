import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class)
import Html.Events exposing (on,onClick)
import Array exposing (Array)
import Mouse
import Task
import Time
import Window
import Maybe exposing (..)
import Json.Decode as Json
import Debug

tileSize : Int
tileSize = 50

main =
        Html.program
                { init = init
                , view = view
                , update = update
                , subscriptions = subscriptions
                }

type alias Board = Array (Array Bool)

width : Board -> Int
width board =
        (Array.get 0 board)
        |> (Maybe.map Array.length)
        |> (withDefault 0)

height : Board -> Int
height board = Array.length board

get : Int -> Int -> Board -> Bool
get y x board =
        let
            x = x % (width board)
            y = y % (height board)
        in
        Array.get y board `andThen` (Array.get x)
        |> withDefault False

set : Int -> Int -> Bool -> Board -> Board
set y x val board =
        let
                x = x % (width board)
                y = y % (height board)
        in
                Array.set y (Array.set x val (withDefault Array.empty (Array.get y board))) board

type Msg =
        Click (Int,Int) |
        Tick |
        Resize Window.Size


init : ( Board, Cmd Msg )
init = ( Array.empty, Task.perform (\_ -> Resize {height=0,width=0}) Resize Window.size )

sizeToMsg : Window.Size -> Msg
sizeToMsg x = Resize x

subscriptions : Board -> Sub Msg
subscriptions model =
        Sub.batch [ Time.every Time.second (\t -> Tick), Window.resizes Resize ]

resize : a -> Int -> Array a -> Array a
resize default newLen oldArray =
        let
                len = Array.length oldArray
        in
                if newLen > len
                then Array.append oldArray (Array.repeat (newLen - len) default)
                else Array.slice 0 newLen oldArray

row : Int -> Array Bool
row len = Array.repeat len False

toInt : Bool -> Int
toInt bool = case bool of
                        True -> 1
                        False -> 0

updateTile : Board -> Int -> Int -> Bool -> Bool
updateTile board y x tile =
        let
            count = List.foldl (\(y,x) total -> total + (toInt <| get y x board)) 0 [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
        in case count of
                0 -> False
                1 -> False
                2 -> tile
                3 -> True
                _ -> False

updateRow : Board -> Int -> Array Bool -> Array Bool
updateRow board y row = Array.indexedMap (updateTile board y) row

update : Msg -> Board -> (Board, Cmd Msg)
update msg board =
        (case (Debug.log "msg" msg) of
                Resize r ->
                        let
                                width = r.width // tileSize
                                height = r.height // tileSize
                        in
                                Array.map (resize False width) (resize (row width) height board)
                Click (y, x) ->
                        set y x (not (get x y board)) board
                Tick ->
                        Array.indexedMap (updateRow board) board
        , Cmd.none)

viewTile : Int -> Int -> Bool -> Html Msg
viewTile y x tile =
        Html.td [ onClick (Click (y,x))
                 , class (if tile then "alive" else "dead")
                 ]
                 [text " "]

viewRow : Int -> Array Bool -> Html Msg
viewRow y row =
        Html.tr [] (Array.toList (Array.indexedMap (viewTile y) row))

view : Board -> Html Msg
view board =
        Html.table [] (Array.toList (Array.indexedMap viewRow board))

