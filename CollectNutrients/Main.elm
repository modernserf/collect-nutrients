import Debug
import Dict exposing (Dict)
import Maybe exposing (Maybe(Just, Nothing), andThen, withDefault)
import CollectNutrients.Layout exposing (render)
import CollectNutrients.Actions exposing ( Action(ClickCookie, BuyBuilding, ClockTick,InitAction) )
import CollectNutrients.Building exposing (Building, priceAtCount)
import CollectNutrients.State exposing (State, initState, addCookies, spendCookies, incBuilding, setTime)
import CollectNutrients.Run exposing (runApp)
import CollectNutrients.Clock exposing (clock)
import Debug

main = runApp render reducer initState InitAction [clock]

reducer : Action -> State -> State
reducer action state =
    case (Debug.watch "action" action) of
        ClickCookie -> state |> addCookies (cookiesPerClick state)
        BuyBuilding bldg ->
            Dict.get bldg.id state.buildings
                `andThen` getPrice bldg
                `andThen` filter' ((>) state.cookies)
                `andThen` (\price -> state
                    |> spendCookies price
                    |> incBuilding bldg
                    |> Just)
                |> withDefault state
        ClockTick t -> state
            |> setTime t
            |> addCookies (cookiesPerTick state)
        _ -> state

--TODO
cookiesPerTick : State -> Float
cookiesPerTick state = Dict.foldl
    (\k (bldg, count) sum -> sum + bldg.baseCPS * (toFloat count))
    0.0
    state.buildings

cookiesPerClick : State -> Float
cookiesPerClick state = 1.0

filter' : (a -> Bool) -> a -> Maybe a
filter' cond value = if (cond value) then Just value else Nothing

getPrice bldg (_,count) = Just (priceAtCount bldg count)
