import Control.Monad
import Data.Either
import Text.ParserCombinators.Parsec
import qualified Data.Map as M

data Resource = Resource { quantity :: Int, name :: String } deriving (Show, Eq, Ord)
data Recipe = Recipe { ingredients :: [Resource], produces :: Resource } deriving (Show, Eq, Ord)

resource     = liftM2 Resource (read <$> many1 digit) (space *> many1 upper)
recipe       = liftM2 Recipe (resource `sepBy` string ", ") (string " => " *> resource)
parseRecipes = fromRight [] . parse (recipe `sepEndBy` newline) ""

main = do
    recipes <- foldr save M.empty . parseRecipes  <$> getContents
    print $ search (oreSearch recipes) 1
    where save r@(Recipe _ (Resource _ name)) = M.insert name r
          oreSearch r n = ore r n <= 1000000000000

ore recipes n = flip (M.!) "ORE" . snd $ converge requirements (chems, M.singleton "FUEL" n)
    where requirements a = foldr (react recipes) a (M.keys recipes)
          chems = M.fromList [ (item, 0) | item <- M.keys recipes ]

react recipes item (chemicals,needs) =
    let
          Recipe ingredients (Resource produced _) = recipes M.! item
          has          = M.findWithDefault 0 item chemicals
          needed       = M.findWithDefault 0 item needs
          required     = max 0 (needed - has)
          amount       = (required + produced - 1) `div` produced
          rest         = amount * produced - needed
          leftover     = M.adjust (+ rest) item chemicals
          requirements = M.fromList [ (name, amount * prod) | Resource prod name <- ingredients ]

    in (leftover, M.unionWith (+) requirements $ M.delete item needs)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

search :: (Int -> Bool) -> Int -> Int
search p n = search' n (n + 1)
    where search' lower upper
            | p upper             = search' lower (upper * 2)
            | lower + 1 == upper  = lower
            | p (mid lower upper) = search' (mid lower upper) upper
            | otherwise           = search' lower (mid lower upper)
          mid a b = (a + b) `div` 2
