import Control.Monad
import Data.Either
import Text.ParserCombinators.Parsec
import qualified Data.Map as M

data Resource = Resource { quantity :: Int, name :: String } deriving (Show, Eq, Ord)
data Recipe = Recipe { ingredients :: [Resource], produces :: Resource } deriving (Show, Eq, Ord)

resource     = liftM2 Resource (read <$> many1 digit) (space *> many1 upper)
recipe       = liftM2 Recipe (resource `sepBy` string ", ") (string " => " *> resource)
parseRecipes = fromRight [] . parse (recipe `sepEndBy` newline) ""

main = print . ore . foldr save M.empty . parseRecipes  =<< getContents
    where save r@(Recipe _ (Resource _ name)) = M.insert name r

ore recipes = flip (M.!) "ORE" . snd $ converge requirements (chems, M.singleton "FUEL" 1)
    where requirements a = foldr (react recipes) a (M.keys recipes)
          chems = M.fromList [ (item, 0) | item <- M.keys recipes ]

react recipes item (chemicals,needs) =
    let
          has = M.findWithDefault 0 item chemicals
          needed = M.findWithDefault 0 item needs
          n = max 0 (needed - has)
          (Recipe ingredients (Resource produced _)) = recipes M.! item
          amount = (n + produced - 1) `div` produced
          rest = amount * produced - needed
          leftover = M.adjust (+ rest) item chemicals
          requirements = M.fromList [ (name, amount * prod) | Resource prod name <- ingredients ]

    in (leftover,M.unionWith (+) requirements (M.delete item needs))

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)
