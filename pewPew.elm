-- a very dumb game of asteroids

import Keyboard
import Window
import open Vectors
import List

-- a level, the scene to render
type Scene = 
    { ship:Ship
    , roids:[Roid]
    , bullets:[Particle]
    }

type Positional a = {a | pos:Vec }      {- anything that can be positioned -}

type Particle = {pos:Vec, vel:Vec, timeToLive:Int}          {- a limited-life particle -}
type Ship = {pos:Vec, vel:Vec, angle:Float, thrust:Float, fired:Bool}   {- our protagonist. Angle is [0..1] as tau radians -}
type Roid = {pos:Vec, vel:Vec, size:Int}                    {- an asteroid. Size starts at 3, reduced on split. -}

type ScreenSize = (Int, Int) {- x,y of window dimensions -}
type Controls = (Vec, Bool, ScreenSize)   {- ship thrust and steer, fire button pressed -}

defaultShip = {pos={x=0,y=0},vel={x=0,y=0}, angle=0, thrust=0, fired=False}
randomRoid = {pos={x=100,y=100}, vel={x= -0.2, y= -0.2}, size=3} {- not really very random yet! -}
defaultScene = {ship=defaultShip, roids=[randomRoid], bullets=[]}     {- should include a randomly positioned set of 'roids -}

-- fit a value into a range by wrapping
wrap : Float -> Float -> Float -> Float
wrap lo hi v = if (v >= lo && v <= hi) then (v) else if (v < lo) then (hi - (lo - v)) else (lo + (v - hi))

-- fit a thing into a space
wrapPositional : ScreenSize -> Positional a -> Positional a
wrapPositional (sx,sy) thing =
    let lx = 0 - (toFloat sx / 2)
        ly = 0 - (toFloat sy / 2)
        hx = (toFloat sx / 2)
        hy = (toFloat sy / 2)
    in {thing | pos <- {x=wrap lx hx thing.pos.x, y=wrap ly hy thing.pos.y}}

-- update ship for next frame
shipFrame : Ship -> Ship
shipFrame s =
    let newVel = limit 4 {x=s.vel.x + (s.thrust * sin(turns s.angle)), y=s.vel.y + (s.thrust * -(cos (turns s.angle)))}
        newPos = {x=s.pos.x + newVel.x, y=s.pos.y + newVel.y}
    in  {s | pos <- newPos, vel <- newVel}

-- update the scene for the next frame
sceneFrame : Scene -> Scene
sceneFrame sc = {sc | ship <- shipFrame sc.ship, roids <- map (roidFrame) sc.roids }   {- also do collision etc... -}

-- update a single roid for the frame
roidFrame : Roid -> Roid
roidFrame r = {r | pos <- {x= r.pos.x + r.vel.x, y= r.pos.y + r.vel.y} }

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

-- very rough!!!
bulletCollision : [Roid] -> Particle -> Bool
bulletCollision asteroids bullet =
    let dx2 a b = (a.pos.x - b.pos.x) * (a.pos.x - b.pos.x)
        dy2 a b = (a.pos.y - b.pos.y) * (a.pos.y - b.pos.y)
        dist a b = sqrt(dx2 a b + dy2 a b)
    in  all (\x -> x <= 0) ( map (\roid -> (10.0 * toFloat roid.size) - (dist roid bullet) ) (asteroids) )

-- given bullets and one asteroid,
-- if any impacts and roid is size 1: output empty
-- if any impacts and roid size > 1: output two smaller roids
-- otherwise output same roid
roidCollision : [Particle] -> Roid -> [Roid]
roidCollision bullets roid = 
    let isImpact = any (\x-> not x) (map (bulletCollision [roid]) bullets)
        {- note that the child roids' x and y are swapped from parent -}
        leftSmaller = if (roid.size == 1) then ([]) else ([{roid | size <- roid.size - 1, vel <- {x=roid.vel.y * 2, y=roid.vel.x * (-2)}}])
        rightSmaller = if (roid.size == 1) then ([]) else ([{roid | size <- roid.size - 1, vel <- {x=roid.vel.y * (-2), y=roid.vel.x * 2}}])
    in  if (isImpact) then (leftSmaller ++ rightSmaller) else ([roid])

-- very inefficient collision detection!
collideFrame : Scene -> Scene
collideFrame f =
    let newBullets = List.filter (bulletCollision (f.roids)) f.bullets
        newRoids   = concatMap (roidCollision f.bullets) f.roids {- note that we impact on the old frame's bullets -}
    in  {f | bullets <- newBullets, roids <- newRoids } 

-- Apply input to the scene
handleInput : Controls -> Scene -> Scene
handleInput (accel, guns, screensize) s =
    let gunCanFire = guns && (not s.ship.fired)
    in  (collideFrame . sceneFrame) {s |
        ship <- wrapPositional screensize (shipInput accel guns s.ship),
        bullets <- openFire s.ship gunCanFire s.bullets,
        roids <- map (wrapPositional screensize) s.roids} 

-- game input: arrows for thrust, space to shoot
controlInput : Signal Controls
controlInput = sampleOn (fps 20) ((,,) <~ arrowsVector ~ Keyboard.space ~ Window.dimensions)

-- A.run syntax seems weird...
runningScene : Signal Scene
runningScene = foldp (handleInput) defaultScene controlInput 

main : Signal Element
main = drawScene <~ Window.dimensions ~ runningScene

-- draw a space ship, cruising through the aether...
-- todo: draw this nicer
drawShip : Ship -> Form
drawShip s = ngon 3 20 |> filled (rgb 0 85 176) |> move (s.pos.x, s.pos.y) |> rotate (turns s.angle + (pi*1.5))

-- bullets, because (m/2)v^2 is potent.
drawBullet : Particle -> Form
drawBullet b = circle 2 |> filled (rgb 100 100 0) |> move (b.pos.x, b.pos.y)

drawRoid : Roid -> Form
drawRoid r = circle (toFloat (r.size * 9)) |> filled (rgb 50 50 50) |> move (r.pos.x, r.pos.y)

-- collage all the freeform bits together into a renderable element
drawScene : (Int,Int) -> Scene -> Element
--drawScene (w,h) scene = asText {screenx = w, screeny = h, s = scene }
drawScene (w,h) scene = 
    collage w h ([drawShip scene.ship] ++ (map (drawBullet) scene.bullets) ++ (map (drawRoid) scene.roids))
    |> color (rgb 0 0 0)

