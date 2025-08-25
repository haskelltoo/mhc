module Hanjiru.Interpret.Earley.Slow where

import Hanjiru.Prelude

import Hanjiru.Interpret (State)
import Hanjiru.Interpret qualified
import Hanjiru.Language (Move (..))

data SlowEarley f info where

  SlowEarley :: SlowEarley Identity info

instance Hanjiru.Interpret.ParseVia SlowEarley where

  parse SlowEarley = \g input ->
    let
      g' = runIdentity $ retractKnot g
    in
      parse [(freeAp g', input)] [] []

  type Result SlowEarley info a = [[a]]

parse :: Input input t
  => [(State t a, input)]
  -> [a]
  -> [(State t a, input)]
  -> [[a]]

parse [] ress [] = [ress]
parse [] ress next = ress : (parse next [] [])

parse ((state, input):states) ress next =
  case state of
    Return a -> parse states (a:ress) next
  
    Bind (Scan p) k ->
      case uncons input of
        Nothing -> parse states ress next
        Just (t, rest) ->
          case p t of
            Nothing -> parse states ress next
            Just a  -> parse states ress ((k a, rest):next)
  
    Bind (Predict rs) k ->
      let
        states' = [(freeAp r >>= k, input) | r <- rs]
      in
        parse (states ++ states') ress next
