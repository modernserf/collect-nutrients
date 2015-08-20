import Graphics.Element exposing (..)
import Signal exposing ((<~), mailbox, message, sampleOn, foldp, mergeMany)
import Time exposing (Time)
import Graphics.Input exposing (button)
import Debug
import Dict exposing (Dict)
import Maybe
import CollectNutrients.Layout exposing (render)
import CollectNutrients.Actions exposing ( Action(ClickCookie, BuyBuilding, ClockTick,InitAction) )
import CollectNutrients.Building exposing (Building, cursor)
import CollectNutrients.State exposing (State, initState)

main = Signal.map (\s -> render s dispatcher.address) appState

appState : Signal State
appState = foldp reducer initState (mergeMany [clock, dispatcher.signal])

dispatcher : Signal.Mailbox Action
dispatcher = mailbox InitAction

--action creators
clock = ClockTick <~ Time.every Time.second

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



