-- slightly stateful arrows
import Keyboard

-- I must say, I'm not amazingly keen on this
-- record syntax. I guess it matches JS?
sumArrows a b = {x = a.x + b.x, y = a.y + b.y} 

main = lift asText (foldp (sumArrows) {x=0, y=0} Keyboard.arrows)


