
module MonadTransformersStepByStep where

import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map as Map
import           Data.Maybe
import           Data.String
import           Prelude

type Name = String -- variable names
data Exp = Lit Integer -- expressions
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer -- values
           | FunVal Env Name Exp
           deriving (Show)

type Env = Map.Map Name Value -- mapping from names to values

-- our reference (non monadic) implementation
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2 ) = let IntVal i1 = eval0 env e1
                              IntVal i2 = eval0 env e2
                          in IntVal (i1 + i2 )

eval0 env (Abs n e) = FunVal env n e

eval0 env (App e1 e2 ) = let val1 = eval0 env e1
                             val2 = eval0 env e2
                         in case val1 of
                              FunVal env' n body -> eval0 (Map.insert n val2 env') body

