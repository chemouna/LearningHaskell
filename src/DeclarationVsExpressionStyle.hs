module DeclarationVsExpressionStyle where

-- with file

filterdec :: (a -> Bool) -> [a] -> [a]
filterdec p [] = []
filterdec p (x:xs)
  | p x = x : rest
  | otherwise = rest
    where rest = filterdec p xs

filterexp :: (a -> Bool) -> [a] -> [a]
filterexp =
  \p -> \ xs ->
     case xs of
       [] -> []
       (x:xs) ->
         let rest = filterexp p xs
         in if p x
               then x:rest
               else rest


