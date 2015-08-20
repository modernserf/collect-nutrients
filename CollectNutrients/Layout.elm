module CollectNutrients.Layout where
import Graphics.Element exposing (..)
import Color
import Dict
import Graphics.Input exposing (button, customButton)
import Signal exposing (Address, message)
import CollectNutrients.Actions exposing ( Action(ClickCookie, BuyBuilding) )
import CollectNutrients.Building exposing (Building, cursor, priceAtCount)
import CollectNutrients.State exposing (State)

buildingRow : Building -> Float -> Int -> Address Action -> Element
buildingRow bldg cookies count dispatcher =
    let content = buttonContent bldg cookies count
    in  customButton
            (message dispatcher (BuyBuilding bldg))
            content content content

buttonContent bldg cookies count =
    let price = priceAtCount bldg count
        canBuy = cookies > price
    in [
        show (ceiling price),
        show bldg.label,
        show count ]
        |> flow left
        |> container 300 100 middle
        |> color (if canBuy then Color.white else Color.darkGray)

buildingsCol : State -> Address Action -> Element
buildingsCol state dispatcher =
    let list = (Dict.values state.buildings)
    in
        flow down (List.map
            (\(bldg, count) -> buildingRow bldg state.cookies count dispatcher)
            list)

cookieCount : State -> Element
cookieCount state =
    flow right [show (floor state.cookies),
        show "/",
        show (floor state.totalCookies)]

cookieCol state dispatcher =
    flow down [
        cookieCount state,
        button (message dispatcher ClickCookie) "Click"]

render : State -> Address Action -> Element
render state dispatcher =
    flow right [
        cookieCol state dispatcher,
        spacer 400 400,
        buildingsCol state dispatcher
    ]