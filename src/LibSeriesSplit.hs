{-# LANGUAGE TemplateHaskell #-}

module LibSeriesSplit where

import Data.List
import Data.List.Split

-- Exploring Haskell Libraries: The split package

{--

-- splitOn
splitOn "x" "axbxc"

splitOn "," "my,comma,separated,list"

-- endBy

endBy ";" "foo;bar;baz;"

-- splitWhen

splitWhen (< 0) [1,3,-4,5,7,-9,0,2]

-- splitOneOf

splitOneOf ";.," "foo,bar;baz.glurk"

-- split
split (startsWith "app") "applyappicativeapplaudapproachapple"

split (dropDelims $ oneOf ":;") "::abc;:;;fg:h;;ij;"

-- Split a list into chunks of the given lengths:

splitPlaces [2,3,4] [1..20] == [[1,2],[3,4,5],[6,7,8,9]]

-- condense : Condense multiple consecutive delimiters into one.

split (condense $ oneOf "xyz") "aazbxyzcxd"

split (condense . dropInitBlank $ oneOf ":;") "::abc;:;;fg:h;;ij;"

--}

-- split a string in Haskell every 3 letters along
everyThree :: [a] -> [[a]]
everyThree = chunksOf 3

-- everyThree "WEAREDISCOVERED"

-- sort images sizes on both coordinates
-- ["192x192","64x84","96x96","64x64","292x192","32x32","64x12"] in ((pixels)x(pixels))
sortImages :: [String] -> [String]
sortImages = map (intercalate "x") . sort . map (splitOn "x")



