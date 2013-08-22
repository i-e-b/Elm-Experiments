-- a very dumb game of asteroids

import Keyboard
import Automaton as A
import Window

type Vec = {x:Float, y:Float}                               {- simple 2D vector -}
type Particle = {pos:Vec, vel:Vec, timeToLive:Int}          {- a limited-life particle -}
type Ship = {pos:Vec, vel:Vec, angle:Float, thrust:Float, fired:Bool}   {- our protagonist. Angle is [0..1] as tau radians -}
type Roid = {pos:Vec, vel:Vec, size:Int}                    {- an asteroid. Size starts at 3, reduced on split. -}

type Scene = {ship:Ship, roids:[Roid], bullets:[Particle]}  {- a level, the scene to render -}

type Controls = (Vec, Bool)   {- ship thrust and steer, fire button pressed -}

defaultShip = {pos={x=0,y=0},vel={x=0,y=0}, angle=0, thrust=0, fired=False}
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

-- update ship for next frame
shipFrame : Ship -> Ship
shipFrame s =
    let newVel = limit 4 {x=s.vel.x + (s.thrust * sin(turns s.angle)), y=s.vel.y + (s.thrust * -(cos (turns s.angle)))}
        newPos = {x=s.pos.x + newVel.x, y=s.pos.y + newVel.y}
    in  {s | pos <- newPos, vel <- newVel}

-- update the scene for the next frame
sceneFrame : Scene -> Scene
sceneFrame sc = {sc | ship <- shipFrame sc.ship }   {- also do collision etc... -}

-- Apply control input to the ship
shipInput : Vec -> Bool -> Ship -> Ship
shipInput accel gunFired s = {s |
    angle <- wrap 0 1 (s.angle - (accel.x/60)),
    fired <- gunFired,
    thrust <- clamp 0.0 0.1 accel.y}

-- move particles and reduce their lifespan
ageParticles : [Particle] -> [Particle]
ageParticles ps =
    (map (\p -> {p | pos <- {x=p.pos.x+p.vel.x, y=p.pos.y+p.vel.y}, timeToLive <- p.timeToLive - 1}) ps)
    |> filter (\p -> p.timeToLive > 0)

-- add bullets to the list if guns firing, expire dead bullets
openFire : Ship -> Bool -> [Particle] -> [Particle]
openFire ship guns bullets =
    let stillAlive = ageParticles bullets 
        newVel     = {x=ship.vel.x + sin(turns ship.angle)*5, y=ship.vel.y - cos(turns ship.angle)*5}
        newBullet  = {pos=ship.pos, vel=newVel, timeToLive=120}
    in  if (guns && length stillAlive < 16) then (newBullet :: stillAlive) else (stillAlive)

-- Apply input to the scene
handleInput : Controls -> Scene -> Scene
handleInput (accel, guns) s =
    let gunCanFire = guns && (not s.ship.fired)
    in  sceneFrame {s |
        ship <- shipInput accel guns s.ship,
        bullets <- openFire s.ship gunCanFire s.bullets} 

-- arrow keys as a Vec, normalised to magnitude 1
arrowsVector : Signal Vec
arrowsVector = (normalise . toVec) <~ Keyboard.arrows

-- game input: arrows for thrust, space to shoot
controlInput : Signal Controls
controlInput = sampleOn (fps 20) ((,) <~ arrowsVector ~ Keyboard.space)

-- A.run syntax seems weird...
runningScene : Signal Scene
runningScene = foldp (handleInput) defaultScene controlInput 

--main : Signal a
main = drawScene <~ Window.dimensions ~ runningScene

-- draw a space ship, cruising through the aether...
-- todo: draw this nicer
drawShip : Ship -> Form
drawShip s = ngon 3 20 |> filled (rgb 0 85 176) |> move (s.pos.x, s.pos.y) |> rotate (turns s.angle + (pi*1.5))

-- bullets, because (m/2)v^2 is potent.
drawBullet : Particle -> Form
drawBullet b = circle 2 |> filled (rgb 100 100 0) |> move (b.pos.x, b.pos.y)

-- collage all the freeform bits together into a renderable element
drawScene : (Int,Int) -> Scene -> Element
drawScene (w,h) scene = 
    collage w h ([drawShip scene.ship] ++ (map (drawBullet) scene.bullets))
    |> color (rgb 0 0 0)

