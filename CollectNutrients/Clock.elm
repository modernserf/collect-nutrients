module CollectNutrients.Clock where
import Signal exposing ((<~))
import Time exposing (Time)
import CollectNutrients.Actions exposing ( Action(ClockTick) )

clock = ClockTick <~ Time.every Time.second
