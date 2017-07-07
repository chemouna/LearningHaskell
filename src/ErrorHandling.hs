
module ErrorHandling where

import qualified Control.Exception as E

-- using error
myDiv1 :: Float -> Float -> Float
myDiv1 x 0 = error "Division by zero"
myDiv1 x y = x / y

-- using catch
myDiv2 :: Float -> Float -> Float
myDiv2 = E.catch
