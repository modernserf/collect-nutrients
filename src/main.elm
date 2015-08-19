import Graphics.Element exposing (..)
import Signal exposing ((<~), mailbox, message, sampleOn, foldp, mergeMany)
import Time exposing (Time)
import Graphics.Input exposing (button)
import Debug
import Dict exposing (Dict)
import Maybe

main = render <~ appState

appState : Signal State
appState = foldp reducer initState (mergeMany [clock, dispatcher.signal])

dispatcher : Signal.Mailbox Action
dispatcher = mailbox InitAction

--actions
type Action
    = ClickCookie
    | BuyBuilding Building
    --| BuyUpgrade UpgradeID Time
    | ClockTick Time
    | InitAction

--action creators
clock = ClockTick <~ Time.every Time.second

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

--state
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

reducer : Action -> State -> State
reducer action state =
    case (Debug.watch "action" action) of
        ClickCookie ->
            let cookies = (cookiesPerClick state)
            in { state
                | cookies <- state.cookies + cookies
                , totalCookies <- state.totalCookies + cookies
            }
        BuyBuilding bldg -> buy state bldg
        ClockTick t ->
            let cookies = (cookiesPerTick state)
            in { state
                    | time <- t
                    , cookies <- state.cookies + cookies
                    , totalCookies <- state.totalCookies + cookies
                }
        _ -> state

--TODO
cookiesPerTick : State -> Float
cookiesPerTick state = Dict.foldl
    (\k (bldg, count) sum -> sum + bldg.baseCPS * (toFloat count))
    0.0
    state.buildings



cookiesPerClick : State -> Float
cookiesPerClick state = 1.0

growthConstant = 1.15

buy : State -> Building -> State
buy state bldg =
    case (Dict.get bldg.id state.buildings) of
        Nothing -> state
        Just (_, count) ->
            let price = (toFloat bldg.baseCost) *
                (growthConstant ^ (toFloat count))
            in
                if  state.cookies < price then state else
                    { state
                        | cookies <- state.cookies - price
                        , buildings <-
                            Dict.update
                                bldg.id
                                (Maybe.map (\(_,x) -> (bldg, x + 1)))
                                state.buildings }



--views
render : State -> Element
render state =
    container 640 480 middle (flow down [
        (show  (floor state.cookies)),
        (show (floor state.totalCookies)),
        button (message dispatcher.address ClickCookie) "Click",
        button (message dispatcher.address (BuyBuilding cursor))
                "Cursor"
    ])




