module State where

import Data.Word
import Data.Vector as V

import Op
import Tape

data State = State { ip :: Int
                   , dp :: Int
                   , mem :: Vector Word8  } deriving (Show, Eq)

new :: State
new = State 0 0 (V.replicate 8 0)

memAt = flip $ (V.!) . mem

incrIp :: State -> State
incrIp s = s { ip = ip s + 1 }

stepop :: Op -> Tape -> State -> State
stepop op t s =
  let dval = mem s ! dp s
      len = V.length . mem $ s in
  case op of
    Add -> incrIp $ s { mem = mem s // [(dp s, dval + 1)] }
    Sub -> incrIp $ s { mem = mem s // [(dp s, dval - 1)] }
    PRight -> incrIp $ s { dp = mod (dp s + 1) len }
    PLeft -> incrIp $ s { dp = mod (dp s - 1 + len) len }
    -- _ -> remaining


step :: Tape -> State -> Maybe State
step tape state = do
  o <- tape !? ip state
  Just $ stepop o tape state
