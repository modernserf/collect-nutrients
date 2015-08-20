module CollectNutrients.Building where
import Dict exposing (Dict)
type alias BuildingID = Int

type alias Building  = {
    id: BuildingID,
    baseCost: Int,
    baseCPS: Float
}

cursor = { id = 1, baseCost = 15, baseCPS = 0.1 }
grandma = { id = 2, baseCost = 100, baseCPS = 1.0 }

initBuildings : List Building -> Dict BuildingID (Building, Int)
initBuildings list = list
    |> List.map (\g -> (g.id , (g, 0)))
    |> Dict.fromList
