{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskell where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibQ :: Int -> Q Exp
fibQ n = [| fibs !! n |]

--

isPrime :: (Integral a) => a -> Bool
isPrime k | k <=1 = False | otherwise = not $ elem 0 (map (mod k)[2..k-1])

nextPrime :: (Integral a) => a -> a
nextPrime n | isPrime n = n | otherwise = nextPrime (n+1)

doPrime :: (Integral a) => a -> a -> [a]
doPrime n m
        | curr > m = []
        | otherwise = curr:doPrime (curr+1) m
        where curr = nextPrime n

primeQ :: Int -> Int -> Q Exp
primeQ n m = [| doPrime n m |]

-- using mkName

g = let pi = 3 in $([| pi + $(varE (mkName "pi")) |]) -- g = 3 + pi = 6

-- using newName

f1 = $(do
  nm1 <- newName "x"
  let nm2 = mkName "x"
  return (LamE [VarP nm1] (LamE [VarP nm2] (VarE nm1)))
 )
 -- produces f = \x0 -> \x -> x0


-- c printf  with TH

-- Possible string tokens: %d %s and literal strings
data Format = D | S | L String
    deriving Show

-- a poor man's tokenizer
tokenize :: String -> [Format]
tokenize [] = []
tokenize ('%':c:rest) | c == 'd' = D : tokenize rest
                      | c == 's' = S : tokenize rest
tokenize (s:str) = L (s:p) : tokenize rest -- so we don't get stuck on weird '%'
    where (p,rest) = span (/= '%') str

-- generate argument list for the function
args :: [Format] -> [PatQ]
args fmt = concatMap (\(f,n) -> case f of
                                  L _ -> []
                                  _   -> [varP n]) $ zip fmt names
    where names = [ mkName $ 'x' : show i | i <- [0..] ]

-- generate body of the function
body :: [Format] -> ExpQ
body fmt = foldr (\ e e' -> infixApp e [| (++) |] e') (last exps) (init exps)
    where exps = [ case f of
                    L s -> stringE s
                    D   -> appE [| show |] (varE n)
                    S   -> varE n
                 | (f,n) <- zip fmt names ]
          names = [ mkName $ 'x' : show i | i <- [0..] ]

-- glue the argument list and body together into a lambda
-- this is what gets spliced into the haskell code at the call
-- site of "printf"
printf :: String -> Q Exp
printf format = lamE (args fmt) (body fmt)
    where fmt = tokenize format


    
