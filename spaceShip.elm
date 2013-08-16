-- very simple spaceship simulation

import Keyboard
import Automaton as A

type Vec = {x:Float, y:Float}

mag : Vec -> Float
mag v = sqrt (v.x * v.x + v.y * v.y)

normalise : Vec -> Vec
normalise v = 
    let m = mag v
    in  if (m == 0.0) then v else {x = v.x / m, y = v.y / m}

limit : Float -> Vec -> Vec
limit n v =
    let m = (mag v) / n
    in  if (m <= 1.0) then v else {x=v.x/m, y=v.y/m}

toVec : {x:Int, y:Int} -> Vec
toVec r = {x=toFloat r.x, y=toFloat r.y}

shipVelocity : Vec -> Vec -> Vec
shipVelocity accel currVel = limit 20 {x = (currVel.x + accel.x), y = (currVel.y + accel.y)}

-- state machine taking acceleration vector, giving velocity vector
ship : A.Automaton Vec Vec
ship = A.state {x = 0.0, y = 0.0} shipVelocity

normalArrows : Signal Vec
normalArrows = (normalise . toVec) <~ Keyboard.arrows

main = (asText) <~ (A.run ship {x=0.0,y=0.0} normalArrows)
