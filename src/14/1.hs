#!/usr/bin/env runhaskell
import qualified Data.Map as M
import Control.Monad
import Data.Maybe
import Data.Either
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

data Resource = Resource Int String deriving (Show, Eq, Ord)
data Recipe = Recipe { ingredients :: [Resource], product :: Resource } deriving (Show, Eq, Ord)

resource = liftM2 Resource (read <$> many1 digit) (space *> many1 upper) 
recipe   = liftM2 Recipe (resource `sepBy` string ", ") (string " => " *> resource)
recipes  = fromRight [] . parse (recipe `sepEndBy` newline) ""

main = print . flip cost (Resource 1 "FUEL") . foldr save M.empty . recipes  =<< getContents 
    where save (Recipe i (Resource _ name)) = M.insert name i

cost recipes (Resource x "ORE") = x
cost recipes (Resource x name) = sum . map (cost recipes) . fromJust $  M.lookup name recipes
