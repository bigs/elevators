{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Cole.Elevators where

import Control.Lens
import Control.Lens.TH
import Data.List (sortBy, sort)
import qualified Data.Map as M

type Floor = Int
type ElevatorID = Int
type ElevatorCall = (Floor, Direction)

data Direction = Up | Down | Stationary deriving (Show, Eq)

invertDirection :: Direction -> Direction
invertDirection Up = Down
invertDirection Down = Up
invertDirection x = x

data Elevator = Elevator {
  _elevatorID           :: ElevatorID,
  _elevatorDirection    :: Direction,
  _elevatorFloor        :: Floor,
  _elevatorDests        :: [Floor],
  _elevatorReturnDests  :: [Floor]
  } deriving (Show, Eq)
makeLenses ''Elevator

-- a is the control system type, s is its state representation
class ElevatorControlSystem a s | a -> s where
  -- status returns a status representation of the elevator bank state
  status :: a -> s

  -- pickup instantaneously chooses the optimal elevator and places the
  -- passenger in question on the pickup queue for that elevator
  pickup :: ElevatorCall -> a -> a

  -- updates an elevator by its id. this will usually reflect a new destination.
  update :: Elevator -> a -> a

  -- step advances the simulation one step, returning the new state of
  -- the elevators
  step   :: a -> a

type ElevatorBank = M.Map ElevatorID Elevator

callIsOnPath :: ElevatorCall -> Elevator -> Bool
callIsOnPath (fl, dir) e@(Elevator _ edir efl _ _)
  | edir == dir = case dir of
      Up   -> efl <= fl
      Down -> efl >= fl
      _    -> True
  | otherwise = False

distanceFromCall :: ElevatorCall -> Elevator -> Int
distanceFromCall (fl, _) e = abs (fl - efl)
  where efl = e ^. elevatorFloor

compareByView :: Ord b => (a -> b) -> a -> a -> Ordering
compareByView f x y = compare (f x) (f y)

sortByView :: Ord b => (a -> b) -> [a] -> [a]
sortByView f = sortBy (compareByView f)

setElevatorDirectionIfNecessary :: Direction -> Elevator -> Elevator
setElevatorDirectionIfNecessary dir = elevatorDirection %~ go
  where go Stationary = dir
        go x = x

addCallToElevator :: ElevatorCall -> Elevator -> Elevator
addCallToElevator (fl, Up) = setElevatorDirectionIfNecessary Up .
  (elevatorDests %~ (sort . (fl :)))
addCallToElevator (fl, Down) = setElevatorDirectionIfNecessary Down .
  (elevatorDests %~ (reverse . sort . (fl :)))
addCallToElevator _ = id

addCallToElevatorReturn :: ElevatorCall -> Elevator -> Elevator
addCallToElevatorReturn (fl, Down) = elevatorReturnDests %~ (sort . (fl :))
addCallToElevatorReturn (fl, Up) =
  elevatorReturnDests %~ (reverse . sort . (fl :))
addCallToElevatorReturn _ = id

stepElevator :: Elevator -> Elevator
stepElevator e@(Elevator eid dir fl dests rDests) = Elevator eid dir' fl' dests' rDests'
  where floorStepper = case dir of
          Up -> succ :: Int -> Int
          Down -> pred :: Int -> Int
          Stationary -> id
        fl' = floorStepper fl
        dir' = case dests of
          [] -> case rDests of
            [] -> Stationary
            _  -> invertDirection dir
          _ -> dir
        (dests', rDests') = case dests of
          []     -> (rDests, [])
          (x:[]) -> if x == fl' then (rDests, []) else (dests, rDests)
          (x:xs) -> if x == fl' then (xs, rDests) else (dests, rDests)

instance ElevatorControlSystem ElevatorBank [Elevator] where
  status es = map snd $ M.toList es

  pickup call es = case filter (callIsOnPath call) elist of
    [] -> let e   = head elist
              eid = e ^. elevatorID in
          M.adjust (addCallToElevatorReturn call) eid es
    xs -> let e   = head (sortByView (distanceFromCall call) xs)
              eid = e ^. elevatorID in
          M.adjust (addCallToElevator call) eid es
    where elist = status es

  update e es = M.insert eid e es
    where eid = e ^. elevatorID

  step es = M.map stepElevator es 
