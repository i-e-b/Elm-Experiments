-- slightly stateful arrows
import Keyboard
import Automaton

-- I must say, I'm not amazingly keen on this
-- record syntax. I guess it matches JS?
sumArrows a b = {x = a.x + b.x, y = a.y + b.y} 

periodicArrows = sampleOn (fps 20) Keyboard.arrows

-- use foldp to move a point from {0,0} based on the arrows
movePoint = (foldp (sumArrows) {x=0, y=0} periodicArrows) 

main = lift asText movePoint


