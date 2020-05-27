module SecuCloud where


import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           Data.Maybe
import           Data.Vector                    ( Vector
                                                , (!?)
                                                )
import           Debug.Trace
import           Safe

import qualified Data.Vector                   as V


-- | A bunny hop.
data BunnyHop = BunnyHop {
    cField  :: Int  -- ^ current index field in the game (must not be negative)
  , jump    :: Int  -- ^ jump value, this could be negative
  , bunny   :: Int  -- ^ bunny index
} deriving (Show, Eq)


-- | A game event.
data Event = Destroy Int
           | Escape Int
           | Suicide Int
  deriving (Show, Eq)

isEscape :: Event -> Bool
isEscape (Escape _) = True
isEscape _          = False


runGame :: Int    -- ^ number of bunnies
        -> [Int]  -- ^ game field
        -> Int
runGame bs gf' =
  let gf    = V.fromList gf'
      bhops = fmap (bunnyHops gf) [1 .. bs]
  in  length . filter isEscape . evalGame bhops 500 $ gf


bunnyHops :: Vector Int  -- ^ game field
          -> Int         -- ^ bunny identifier
          -> [BunnyHop]
bunnyHops gf bi =
  catMaybes . takeWhile isJust . iterate (maybe Nothing step) $ event (bi - 1)
 where
  event :: Int -> Maybe BunnyHop
  event pos =
    let cField = pos
        mJump  = gf !? pos
        bunny  = bi
    in  maybe Nothing (\jump -> Just BunnyHop { .. }) mJump

  step :: BunnyHop -> Maybe BunnyHop
  step BunnyHop {..} = event (cField + jump)


-- | We evaluate a game by scanning the events of all bunnies simultaneously.
--
-- This also means we could parallelise the BunnyHop list construction,
-- ideally streaming it.
--
-- Events have no particularly guaranteed order.
evalGame :: [[BunnyHop]]  -- ^ bunny hops for different bunnies
         -> Int           -- ^ max iterations after which we kill
                          --   all remaining bunnies
         -> Vector Int    -- ^ game field
         -> [Event]
evalGame xs maxIter gf = go xs maxIter
 where
  go :: [[BunnyHop]] -> Int -> [Event]
  go [] _ = []
  go xs i
    | -- TODO: logic: detecting cycles would be nicer
      i <= 0 = fmap (Suicide . bunny) (join . fmap (fst . splitAt 1) $ xs)
    | otherwise = let s = fmap (splitAt 1) xs
                      (bh, r) = (fmap fst s, fmap snd s)
                  -- TODO: inefficiency: (++)
                  in  evalHop (join bh) ++ go r (i - 1)

  evalHop :: [BunnyHop] -> [Event]
  evalHop [] = []
  evalHop xs =
    let ts                = fmap targetStone xs
        (escaped, others) = partition (\(s, _) -> isNothing s) ts
        escapedBunnies    = fmap (\(_, y) -> Escape y) escaped
        -- TODO: inefficiency: this might be faster in ST imperatively?
        destroyedBunnies =
            catMaybes
              . fmap
                  (\case
                    xs@(_:_:_) -> Just (fmap (\x -> Destroy $ snd x) xs)
                    _          -> Nothing
                  )
              . group
              . sortBy (\(a, _) (b, _) -> compare a b)
              . fmap (\(x, y) -> (fromJust x, y))
              $ others
   -- TODO: inefficiency: (++)
    in  escapedBunnies ++ join destroyedBunnies

  -- if outside of field, fst is Nothing
  targetStone :: BunnyHop -> (Maybe Int, Int) -- ^ fst is stone, snd is bunny
  targetStone BunnyHop {..} = (, bunny) $ gf !? (cField + jump)

