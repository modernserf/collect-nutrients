module CollectNutrients.Layout where

import Graphics.Element exposing (Element, container, flow, show, middle, down)
import Graphics.Input exposing (button)
import Signal exposing (Address, message)
import CollectNutrients.Actions exposing ( Action(ClickCookie, BuyBuilding) )
import CollectNutrients.Building exposing (cursor)
import CollectNutrients.State exposing (State)

render : State -> Address Action -> Element
render state dispatcher =
    container 640 480 middle (flow down [
        (show  (floor state.cookies)),
        (show (floor state.totalCookies)),
        button (message dispatcher ClickCookie) "Click",
        button (message dispatcher (BuyBuilding cursor))
                "Cursor"
    ])