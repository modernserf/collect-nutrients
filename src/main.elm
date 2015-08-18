import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing ((<~), sampleOn, foldp, mergeMany)
import Mouse
import Dict exposing (Dict)
import Maybe exposing (withDefault)

main : Signal Element
main = render <~ appState

appState = foldp (combineReducerDict reducers) initState (mergeMany [clicks])

combineReducerDict reducers action =
    Dict.map (\k thisState ->
            case (Dict.get k reducers) of
                Just r -> (r action thisState)
                _ -> thisState)


--actions
type Action = Click (Int,Int)

clicks = Click <~ sampleOn Mouse.clicks Mouse.position

--reducers
initState = Dict.fromList [("creature",(0,0))]

reducers = Dict.fromList [("creature",creaturePos)]

type alias CreatureState = (Float,Float)
creaturePos : Action -> CreatureState -> CreatureState
creaturePos action state =
    case action of
        Click (x, y) -> (toFloat (x - 400), toFloat (400 - y))
        _ -> state


--views
render state =
    container 800 800 middle (collage 800 800
        [move ((Dict.get "creature" state) |> withDefault (0,0)) creatureForm])

creatureForm : Form
creatureForm = circle 40 |> filled red