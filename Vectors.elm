-- 2D vector stuff for elm

module Vectors (Vec, mag, normalise, limit, toVec) where

type Vec = {x:Float, y:Float}                               {- simple 2D vector -}

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

