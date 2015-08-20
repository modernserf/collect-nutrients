module CollectNutrients.Run where
import Signal exposing (Signal, Mailbox, Address, mailbox, mergeMany, foldp)
import Graphics.Element exposing (Element)

initDispatcher : a -> Mailbox a
initDispatcher initAction = mailbox initAction

type alias Renderable a b = b -> Address a -> Element
type alias Reducer a b = a -> b -> b

runApp : Renderable a b -> Reducer a b -> b -> a -> List (Signal a) -> Signal Element
runApp render reducer initState initAction signals =
    let dispatcher = initDispatcher initAction
        signal = mergeMany (dispatcher.signal :: signals)
        doRender = (\s -> render s dispatcher.address)
    in
        Signal.map doRender (foldp reducer initState signal)
