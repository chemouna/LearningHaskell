module FindingRectangles where

-- http://www.twanvl.nl/blog/haskell/finding-rectangles

-- Problem : Given a binary image, find the largest axis aligned rectangle that consists only of foreground pixels.

-- An image is a 2D list of booleans, True is the
-- foreground & False is the background
type Image = [[Bool]]

-- An axis aligned rectangle
data Rect = Rect { left, top, width, height :: Int }
              deriving (Eq, Ord, Show)

imWidth, imHeight :: Image -> Int
imHeight = length
imWidth (x:_) = length x
imWidth [] = 0

area, perimeter :: Rect  -> Int
area rect = width rect * height rect
perimeter rect = 2 * width rect + 2 * height rect

-- an image 'contains' a rectangle if all pixels inside the rectangle are foreground pixels.
contains :: Image -> Rect -> Bool
contains im (Rect x y w h) = and pixelsInRect
  where
    pixelsInRect = concatMap cols (rows im)
    rows = take h . drop y . (++repeat [])
    cols = take w . drop x . (++repeat False)
