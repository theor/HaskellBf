module State where

import Data.Word
import Data.Vector as V

import Op
import Tape

data State = State { ip :: Int
                   , dp :: Int
                   , mem :: Vector Word8  } deriving (Show)

new :: State
new = State 0 0 (V.replicate 8 0)

incrIp :: State -> State
incrIp s = s { ip = ip s + 1 }

stepop :: Op -> Tape -> State -> Maybe State
stepop Add t s =
  let dval = mem s ! dp s in
  Just . incrIp $ s { mem = mem s // [(dp s, dval + 1)] }

step :: Tape -> State -> Maybe State
step tape state = do
  o <- tape !? ip state
  stepop o tape state
