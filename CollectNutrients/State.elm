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
    buildings = (initBuildings [cursor, grandma])  }