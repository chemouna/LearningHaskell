
module WidgetRepairs where

-- widgetRepairs :: [Integer] -> Integer -> Integer
widgetRepairs arrivals nd = foldr fn 0 arrivals
  where
    fn acc x = if x == 0 then acc else acc + (if ceil x nd == 0 then 0 else ceil x nd)
    ceil x nd = fromIntegral (ceiling (x / nd))

{--
what is the unknown : nb of days for widget repairs that happens through times 
data ?
   - shop repairs 8 per day
   - how many arrive each day

a list and every time something is consumed -> increase by

keep doing  map (\(x, y) -> if x > y then x - y else 0) $ zip list [8,8,8, 8, 8]
 until list becomes empty

-> how to count that then ?

let l = [10, 0, 0, 4, 20]
let ml =  map (\(x, y) -> if x > y then x - y else 0) $ zip l [8,8,8, 8, 8]

--}
