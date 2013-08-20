-- a very dumb game of asteroids

import Keyboard
import Automaton as A
import Window

type Vec = {x:Float, y:Float}                               {- simple 2D vector -}
type Particle = {pos:Vec, vel:Vec, timeToLive:Int}          {- a limited-life particle -}
type Ship = {pos:Vec, vel:Vec, angle:Float, thrust:Float}   {- our protagonist. Angle is [0..1] as tau radians -}
type Roid = {pos:Vec, vel:Vec, size:Int}                    {- an asteroid. Size starts at 3, reduced on split. -}

type Scene = {ship:Ship, roids:[Roid], bullets:[Particle]}  {- a level, the scene to render -}

defaultShip = {pos={x=0,y=0},vel={x=0,y=0}, angle=0, thrust=0}
defaultScene = {ship=defaultShip, roids=[], bullets=[]}     {- should include a randomly positioned set of 'roids -}

-- Magnitude of a vector
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

-- x,y ints to a Vec
toVec : {x:Int, y:Int} -> Vec
toVec r = {x=toFloat r.x, y=toFloat r.y}

-- fit a value into a range by wrapping
wrap : Float -> Float -> Float -> Float
wrap lo hi v = if (v >= lo && v <= hi) then (v) else if (v < lo) then (hi - (lo - v)) else (lo + (v - hi))

-- fit a value into a range by saturation
pin : Float -> Float -> Float -> Float
pin lo hi v = if (v < lo) then (lo) else if (v > hi) then hi else v

-- update ship for next frame
shipFrame : Ship -> Ship
shipFrame s =
    let newVel = limit 4 {x=s.vel.x + (s.thrust * sin(2*pi*s.angle)), y=s.vel.y + (s.thrust * (0 - (cos (2*pi*s.angle))))}
        newPos = {x=s.pos.x + newVel.x, y=s.pos.y + newVel.y}
    in  {s | pos <- newPos, vel <- newVel}

-- update the scene for the next frame
sceneFrame : Scene -> Scene
sceneFrame sc = {sc | ship <- shipFrame sc.ship }   {- also do collision etc... -}

shipInput : Vec -> Ship -> Ship
shipInput accel s = {s | angle <- wrap 0 1 (s.angle - (accel.x/60)),  thrust <- pin 0.0 0.1 accel.y}

-- Apply input to the scene
handleInput : Vec -> Scene -> Scene
handleInput accel s = sceneFrame {s | ship <- shipInput accel s.ship} 

-- state machine taking frame inputs, giving current ship status
-- The state automaton seems to duplicate the initial state with the run function
sceneState : A.Automaton Vec Scene
sceneState = A.state defaultScene handleInput

-- arrow keys as a Vec, normalised to magnitude 1
periodArrowKeyVec : Signal Vec
periodArrowKeyVec = (normalise . toVec) <~ (sampleOn (fps 20) Keyboard.arrows)

-- A.run syntax seems weird...
flyingShip : Signal Scene
flyingShip = (A.run (sceneState) defaultScene periodArrowKeyVec) 



main = drawScene <~ Window.dimensions ~ flyingShip

--drawScene : (Int,Int) -> Scene -> Element
drawScene (w,h) scene = collage w h
       [ ngon 3 20
          |> filled (rgb 0 85 170)
          |> move (scene.ship.pos.x, scene.ship.pos.y)
          |> rotate (2 * pi * scene.ship.angle + (pi*1.5))
       ]

