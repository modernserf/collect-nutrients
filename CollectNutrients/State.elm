module CollectNutrients.State where
import Dict exposing (Dict)
import Time exposing (Time)
import CollectNutrients.Building exposing (Building, BuildingID, cursor, grandma, initBuildings)

type alias State =
    { cookies : Float
    , totalCookies : Float
    , time : Time
    , buildings : Dict BuildingID (Building, Int) }

initState : State
initState = {
    cookies = 0.0,
    totalCookies = 0.0,
    time = 0,
    buildings = initBuildings  }

incRight : (a,Int) -> (a,Int)
incRight (l,r) = (l, r + 1)

addCookies : Float -> State -> State
addCookies count state =
    { state
    | cookies <- state.cookies + count
    , totalCookies <- state.totalCookies + count }

spendCookies : Float -> State -> State
spendCookies count state = { state | cookies <- state.cookies - count }

incBuilding : Building -> State -> State
incBuilding bldg state =
    { state | buildings <-
        Dict.update bldg.id (Maybe.map incRight) state.buildings }

setTime : Time -> State -> State
setTime time state = { state | time <- time }
