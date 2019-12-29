{-# LANGUAGE TemplateHaskell #-}
import Data.Set hiding (drop, map)
import Control.Lens
import Data.List.Split

data Vec3 a = Vec3 { _x :: a, _y :: a, _z :: a } deriving (Show, Eq, Ord)
data Planet = Planet { _pos :: Vec3 Int, _vel :: Vec3 Int } deriving (Show, Eq, Ord)

combine f (Vec3 a b c) (Vec3 x y z) = Vec3 (f a x) (f b y) (f c z)
(<+>) = combine (+)

$(makeLenses ''Vec3)
$(makeLenses ''Planet)

input = [ mkPlanet (-15)     1    4
        , mkPlanet     1 (-10) (-8)
        , mkPlanet  (-5)     4    9
        , mkPlanet     4     6 (-2) ]
    where mkPlanet x y z = Planet (Vec3 x y z) (Vec3 0 0 0)

cycles f = cycles' empty . zip [0..] . map (map pair)
    where pair p = (f $ _pos p, f $ _vel p)
          cycles' _ [] = 0
          cycles' s ((n,y):ys) | member y s = n
                               | otherwise = cycles' (insert y s) ys

main = print . foldl1 lcm $ map (flip cycles $ iterate simulate input) [_x,_y,_z]

simulate planets =
   let move planet = over pos (<+> _vel planet) planet
       speed v = over vel (<+> v)
       pairs = map (foldl1 (<+>)) . chunksOf 4
             $ [ gravity a b | a <- planets, b <- planets]

   in zipWith ((move .) . speed) pairs planets

gravity a b = combine ((toInt .) . compare) (_pos b) (_pos a)
    where toInt GT = 1
          toInt EQ = 0
          toInt LT = -1
