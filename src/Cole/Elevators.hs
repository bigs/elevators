{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Cole.Elevators where

-- convenience of access/ updateds
import Control.Lens
import Control.Lens.TH

-- sorts for laziness, nub for deduping
import Data.List (sortBy, sort, nub)

-- map to store our elevator bank
import qualified Data.Map as M

-- type aliases so our signatures can tell stories
type Floor = Int
type ElevatorID = Int
type ElevatorCall = (Floor, Direction)

-- elevator states & utility fn
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

-- determine whether an elevator call is on the current path of an elevator
callIsOnPath :: ElevatorCall -> Elevator -> Bool
callIsOnPath (fl, dir) e@(Elevator _ edir efl _ _)
  | edir == dir = case dir of
      Up   -> efl <= fl
      Down -> efl >= fl
      _    -> True
  | otherwise = False

-- scalar - what is the distance of the elevator to the calling passenger
distanceFromCall :: ElevatorCall -> Elevator -> Int
distanceFromCall (fl, _) e = abs (fl - efl)
  where efl = e ^. elevatorFloor

-- utility fns for arbitrary sorts
compareByView :: Ord b => (a -> b) -> a -> a -> Ordering
compareByView f x y = compare (f x) (f y)

sortByView :: Ord b => (a -> b) -> [a] -> [a]
sortByView f = sortBy (compareByView f)

-- when stepping, invert the elevator direction if it was previously stationary
setElevatorDirectionIfNecessary :: Direction -> Elevator -> Elevator
setElevatorDirectionIfNecessary dir = elevatorDirection %~ go
  where go Stationary = dir
        go x = x

-- add a destination to the elevator's list
addCallToElevator :: ElevatorCall -> Elevator -> Elevator
addCallToElevator (fl, Up) = setElevatorDirectionIfNecessary Up .
  (elevatorDests %~ (sort . nub . (fl :)))
addCallToElevator (fl, Down) = setElevatorDirectionIfNecessary Down .
  (elevatorDests %~ (reverse . sort . nub . (fl :)))
addCallToElevator _ = id

-- add a destination to the return list i.e. when it inverts direction
addCallToElevatorReturn :: ElevatorCall -> Elevator -> Elevator
addCallToElevatorReturn (fl, Down) = elevatorReturnDests %~ (sort . (fl :))
addCallToElevatorReturn (fl, Up) =
  elevatorReturnDests %~ (reverse . sort . (fl :))
addCallToElevatorReturn _ = id

-- the elevator stepping function. this implementation evaluates the remaining
-- destinations, adjusts the floor and determines if it needs to reverse its
-- direction.
stepElevator :: Elevator -> Elevator
stepElevator e@(Elevator eid dir fl dests rDests) = Elevator eid dir' fl' dests' rDests'
  where floorStepper = case dir of
          Up -> succ :: Int -> Int
          Down -> pred :: Int -> Int
          Stationary -> id
        fl' = floorStepper fl
        idir = invertDirection dir
        (dir', dests', rDests') = case dests of
          [] -> case rDests of
            [] -> (Stationary, dests, rDests)
            _  -> (idir, rDests, [])
          (x:[]) -> if x == fl' then (idir, rDests, []) else (dir, dests, rDests)
          (x:xs) -> if x == fl' then (dir, xs, rDests) else (dir, dests, rDests)

-- type class instance for our elevator bank
instance ElevatorControlSystem ElevatorBank [Elevator] where
  -- the status is just a list of elevators. simple value projection.
  status es = map snd $ M.toList es

  -- the pick up function:
  -- - checks to see if the call is on the path of any elevator
  --   - if so, add it to the destination list of the closest elevator
  -- - if not, lazily add it to the return path of the first elevator.
  --   ideally, this should be implemented via a similar search, seeing which
  --   elevator we estimate will be closest after completing its voyage
  --
  -- this function works by mutating the destination list of the chosen elevator
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

  -- call the step function
  step es = M.map stepElevator es 
