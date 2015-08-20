module CollectNutrients.Building where
import Dict exposing (Dict)
type alias BuildingID = Int

type alias Building  = {
    id: BuildingID,
    baseCost: Int,
    baseCPS: Float,
    label: String
}

cursor =   { id = 1, label = "Cursor",   baseCost = 15,    baseCPS = 0.1 }
grandma =  { id = 2, label = "Grandma",  baseCost = 100,   baseCPS = 1.0 }
farm =     { id = 3, label = "Farm",     baseCost = 500,   baseCPS = 4   }
factory =  { id = 4, label = "Factory",  baseCost = 3000,  baseCPS = 10  }
mine =     { id = 5, label = "Mine",     baseCost = 10000, baseCPS = 40  }
shipment = { id = 6, label = "Shipment", baseCost = 40000, baseCPS = 100 }

initBuildings = [cursor, grandma,farm,factory,mine,shipment]
    |> List.map (\g -> (g.id , (g, 0)))
    |> Dict.fromList

growthConstant = 1.15

priceAtCount : Building -> Int -> Float
priceAtCount bldg count =
    (toFloat bldg.baseCost) * (growthConstant ^ (toFloat count))

