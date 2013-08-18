-- very simple spaceship simulation

import Keyboard
import Automaton as A

type Vec = {x:Float, y:Float}
type Ship = {pos:Vec, vel:Vec, angle:Float, thrust:Float}
defaultShip = {pos={x=0,y=0},vel={x=0,y=0}, angle=0, thrust=0}
-- angle is [0..1] as tau radians

mag : Vec -> Float
mag v = sqrt (v.x * v.x + v.y * v.y)

-- Normalise a vector to magnitude = 1.0
normalise : Vec -> Vec
normalise v = 
    let m = mag v
    in  if (m == 0.0) then v else {x = v.x / m, y = v.y / m}

-- Limit a vector to a given magnitude
limit : Float -> Vec -> Vec
limit n v =
    let m = (mag v) / n
    in  if (m <= 1.0) then v else {x=v.x/m, y=v.y/m}

toVec : {x:Int, y:Int} -> Vec
toVec r = {x=toFloat r.x, y=toFloat r.y}

wrap : Float -> Float -> Float -> Float
wrap lo hi v = if (v >= lo && v <= hi) then (v) else if (v < lo) then (hi - (lo - v)) else (lo + (v - hi))

-- update ship based for next frame
shipFrame : Ship -> Ship
shipFrame s = s

-- Apply input to the ship
shipInput : Vec -> Ship -> Ship
shipInput accel s = {s | angle <- wrap 0 1 (s.angle + (accel.x/100)),  thrust <- accel.y} 

-- state machine taking frame inputs, giving current ship status
-- The state automaton seems to duplicate the initial state with the run function
ship : A.Automaton Vec Ship
ship = A.state defaultShip shipInput

periodArrowKeyVec : Signal Vec
periodArrowKeyVec = (normalise . toVec) <~ (sampleOn (fps 20) Keyboard.arrows)

main = (asText) <~ (A.run ship defaultShip periodArrowKeyVec)


