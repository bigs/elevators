# Elevator Controller

## Building

This is a Haskell project built with [stack](https://www.stackage.org/). To
build the project, you'll need to download and install stack, after which you
can use the `stack ghci` command to enter a REPL.

## Running

For the sake of getting things done quickly and thoroughly, there is no "main"
function or interactive component to this code. It's simply meant to be used in
the REPL.

## Source

The source for the project can be found in `src/Cole/Elevator.hs`. Each function
and data type should be commented.

## Approach

### Interface Design

The interface presented to programmers is as follows:

```haskell
class ElevatorControlSystem a s | a -> s where
  status :: a -> s
  pickup :: ElevatorCall -> a -> a
  update :: Elevator -> a -> a
  step   :: a -> a
```

Here, `a` is the type of our "control system" or, as I refer to it, "elevator
bank". `s` represents a more friendly projection of the type `a` for examination
via the `status` function (hence the functional dependency of `s` on `a`).

```haskell
type ElevatorCall = (Floor, Direction)
```

`status` is a simple projection.

`pickup`, takes an `ElevatorCall`, represented above, and alters the state of
the elevator bank to reflect which elevator has been chosen to serve the
passenger.

`update` is implemented fairly naively, though it should be plenty sufficient.
It accepts an updated representation of an elevator (presumably reflecting a
passenger having chosen a floor to visit) and updates the state of the elevator
bank to reflect that. In my implementation, it's a simple map entry update.

Note that this entire system is designed to be stateless, for testability.
Mutation and state can easily be handled by an external driver.

### Types & Functionality

As is often time the case with programs written in strongly typed languages, the
general structure of the exercise is dictated by its types. Start with the
`Elevator` type:

```haskell
data Elevator = Elevator {
  _elevatorID           :: ElevatorID,
  _elevatorDirection    :: Direction,
  _elevatorFloor        :: Floor,
  _elevatorDests        :: [Floor],
  _elevatorReturnDests  :: [Floor]
  } deriving (Show, Eq)
```

Each elevator has an ID, memory of the current direction it's travelling in and
the floor it's currently at, and two lists: one of its current destinations, and
another of the destinations it plans to hit on its return trip (i.e.) when it
changes direction.

After some thinking, it seemed convenient - if not necessary - to implement such
a return list. In the event that every elevator in the elevator bank is either
travelling in the direction opposite a passenger wants to go or is travelling in
the same direction but has already passed their floor, this second list gives us
an opportunity to begin scheduling elevator's return trips.

Our elevator bank is simply a map of elevator IDs to elevators:

```haskell
type ElevatorBank = M.Map ElevatorID Elevator
```

The current implementation regarding return trips is naive -- it simply selects
the first elevator in the bank. A stronger implementation, given more time,
would do similar route planning to what is done in the normal destination
planning. It can reuse a lot of the code, but must also factor in estimating the
final destination of an elevator on its current trip (i.e. when do we think the
elevator will drop off its last passenger and reverse direction).

"Normal" destination planning, i.e. selecting an elevator to call when the
passenger IS on the direct path of an elevator, behaves as follows:

- Check to see if there are any elevators heading in the right direction that
  haven't yet passed the passenger
- If so, choose the closest
- If not, add the call to the first elevator's return destinations

The only other significant portion of logic is the "step" evaluator. The step
evaluator operates in stages:

- Update the floor based on which direction we're travelling: increase for up,
  decrease for down, do nothing (identity function) for stationary.
- Update the direction and destination lists based on whether or not we have any
  more destinations in the direction we've been heading, popping off
  destinations as we pass them. If we've exhausted our destination list, reverse
  direction.

### Drawbacks

Right now, as mentioned previously, the return route planning is very naive and
could probably lead to some poor performance in pathological cases.

Furthermore, I'd have to study the domain problem more closely (to help
determine what elevator passengers prefer) in order to optimize the experience
for passengers and overall efficiency.

I didn't have time to implement a driver or simulator, as it seemed out of the
scope of the project. Similarly, I didn't write any unit/property based tests,
as I was aiming for quickness and testing in the REPL.

