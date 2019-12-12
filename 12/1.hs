#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Data.List.Split
import Control.Monad

data Vec3 a = Vec3 { _x :: a, _y :: a, _z :: a } deriving Show
data Planet = Planet { _pos :: Vec3 Int, _vel :: Vec3 Int } deriving Show

combineVec f (Vec3 a b c) (Vec3 x y z) = Vec3 (f a x) (f b y) (f c z)
mapVec f (Vec3 a b c) = Vec3 (f a) (f b) (f c)
(<+>) = combineVec (+)
foldVec f (Vec3 a b c) = a `f` b `f` c
sum' = foldVec (+)

$(makeLenses ''Vec3)
$(makeLenses ''Planet)

input = [ mkPlanet (-15)     1    4
        , mkPlanet     1 (-10) (-8)
        , mkPlanet  (-5)     4    9
        , mkPlanet     4     6 (-2) ]
    where mkPlanet x y z = Planet (Vec3 x y z) (Vec3 0 0 0)

main = print . sum . map energy . (!! 1000) $ iterate simulate input

energy = liftM2 (*) (kinetic . _pos) (kinetic . _vel)
    where kinetic p = sum' (mapVec abs p)

simulate planets =
   let
        move planet = over pos (<+> _vel planet) planet
        speed planet v = over vel (<+> v) planet
        pairs = map (foldl1 (<+>)) . chunksOf 4
              $ [ gravity a b | a <- planets, b <- planets]

   in map move $ zipWith speed planets pairs

gravity a b = combineVec ((toInt .) . compare) (_pos b) (_pos a)
    where toInt GT = 1
          toInt EQ = 0
          toInt LT = -1
