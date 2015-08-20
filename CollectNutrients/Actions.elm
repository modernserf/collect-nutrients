module CollectNutrients.Actions where
import Time exposing (Time)
import CollectNutrients.Building exposing (Building)

type Action
    = ClickCookie
    | BuyBuilding Building
    --| BuyUpgrade UpgradeID Time
    | ClockTick Time
    | InitAction