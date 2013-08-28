-- An experiment in random

import Random
import List
import Mouse

-- pass in an int, get a random.
randList : Int -> Signal Int 
randList i = Random.range 0 255 (constant i)


main : Signal Element
main = asText <~ combine [randList 1, randList 2, randList 3, randList 1]

