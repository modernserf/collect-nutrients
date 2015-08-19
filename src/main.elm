import Graphics.Element exposing (..)
import Signal exposing ((<~), mailbox, message, sampleOn, foldp, mergeMany)
import Time exposing (Time)
import Graphics.Input exposing (button)

main = render <~ appState
appState = foldp reducer initState (mergeMany [clock, dispatcher.signal])

dispatcher : Signal.Mailbox Action
dispatcher = mailbox InitAction

--actions
type Action
    = ClickCookie
    | BuyBuilding Building Int
    --| BuyUpgrade UpgradeID Time
    | ClockTick Time
    | InitAction

--action creators
clock = ClockTick <~ Time.every Time.second


--state
type alias Buildings =
    { cursor : Int
    , grandma : Int
    , farm : Int
    , factory : Int
    , mine : Int }

type alias State =
    { cookies : Float
    , totalCookies : Float
    , time : Time
    , buildings : Buildings }

type alias Building = Buildings -> Int

initState : State
initState = {
    cookies = 0.0,
    totalCookies = 0.0,
    time = 0,
    buildings = {
        cursor = 0,
        grandma = 0,
        farm = 0,
        factory = 0,
        mine = 0
    }}

reducer : Action -> State -> State
reducer action state =
    case action of
        ClickCookie ->
            let cookies = (cookiesPerClick state)
            in { state
                | cookies <- state.cookies + cookies
                , totalCookies <- state.totalCookies + cookies
            }
        BuyBuilding getter qty ->
            let b = state.buildings
                c = b.cursor
            in  { state
                | buildings <- { b | cursor <- c + 1 }
                , cookies <- state.cookies - 15 }
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
cookiesPerTick state = (toFloat state.buildings.cursor) * 0.1

cookiesPerClick : State -> Float
cookiesPerClick state = 1.0


--views
render state =
    container 640 480 middle (flow down [
        (show  (floor state.cookies)),
        (show (floor state.totalCookies)),
        button (message dispatcher.address ClickCookie) "Click",
        (flow left [
            show state.buildings.cursor,
            button (message dispatcher.address (BuyBuilding .cursor 1)) "Cursor"
        ])
    ])




