module Tape where

import Data.Vector as V
import Op
type Tape = Vector Op

new :: Vector a	
new = V.empty
