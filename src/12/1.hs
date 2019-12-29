{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Data.List.Split
import Control.Monad

data Vec3 a = Vec3 { _x :: a, _y :: a, _z :: a } deriving Show
data Planet = Planet { _pos :: Vec3 Int, _vel :: Vec3 Int } deriving Show

combine f (Vec3 a b c) (Vec3 x y z) = Vec3 (f a x) (f b y) (f c z)
apply f (Vec3 a b c) = Vec3 (f a) (f b) (f c)
(<+>) = combine (+)
sum' (Vec3 a b c) = a + b + c

$(makeLenses ''Vec3)
$(makeLenses ''Planet)

input = [ mkPlanet (-15)     1    4
        , mkPlanet     1 (-10) (-8)
        , mkPlanet  (-5)     4    9
        , mkPlanet     4     6 (-2) ]
    where mkPlanet x y z = Planet (Vec3 x y z) (Vec3 0 0 0)

main = print . sum . map energy . (!! 1000) $ iterate simulate input

energy = liftM2 (*) (kinetic . _pos) (kinetic . _vel)
    where kinetic = sum' . apply abs

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
