module Markov (genBars) where

import Prelude

import Data.Maybe (Maybe(..))

import qualified Data.StrMap as M
import qualified Data.Set as S
import qualified Data.String as Str
import qualified Data.List as L

import Data.Tuple (Tuple(..), snd)

import Data.Foldable (foldl)
import Control.Monad.Eff
import qualified Control.Monad.Eff.Random as R


type AccumulatedWordMap = M.StrMap (S.Set String)
type WordMap = M.StrMap (L.List String)
type RandomEff a = forall e. Eff ( random :: R.RANDOM | e ) a
type PairAcc = Tuple String AccumulatedWordMap
type StringAcc = Tuple String (L.List String)


--- Building the word map of possible follower words

updateWordSet :: String -> Maybe (S.Set String) -> Maybe (S.Set String)
updateWordSet newWord maybeFollowerSet =
  case maybeFollowerSet of
    Nothing -> return $ S.singleton newWord
    _ -> map (S.insert newWord) maybeFollowerSet


assocWordMap :: PairAcc -> String -> PairAcc
assocWordMap (Tuple firstWord wordMap) lastWord =
  Tuple lastWord $ M.alter (updateWordSet lastWord) firstWord wordMap


-- | Fold a single line of split words into the accumulated string -> set string map
foldLine :: AccumulatedWordMap -> Array String -> AccumulatedWordMap
foldLine acc words =
  case Data.Array.uncons words of
    Just { head: w, tail: xw } -> snd $ foldl assocWordMap (Tuple w acc) xw
    Nothing -> acc


-- | Construct a word -> List (following words) mapping for an array of crazy bars
toWordMap :: Array (Array String) -> WordMap
toWordMap bars =
  map S.toList $ foldl foldLine M.empty bars


--- Generating stuff

randomListIndex :: forall a. L.List a -> RandomEff (Maybe a)
randomListIndex items =
  map (L.index items) $ R.randomInt 0 $ (L.length items - 1)


nextWord :: WordMap -> String -> RandomEff (Maybe String)
nextWord wordMap precedingWord =
  case M.lookup  precedingWord wordMap of
    Just followingWords -> randomListIndex followingWords
    Nothing -> return Nothing


joinWords :: String -> String -> String
joinWords a b =
  a ++ " " ++ b


genRestLine :: WordMap -> Int -> String -> Maybe String -> RandomEff String
genRestLine _ 0 acc _ = return acc
genRestLine wordMap i acc maybeWord =
  case maybeWord of
    Just word ->
      nextWord wordMap word >>= (genRestLine wordMap (i - 1) $ joinWords acc word)
    Nothing -> return acc


genLine :: WordMap -> L.List String -> RandomEff String
genLine wordMap firstWords =
  randomListIndex firstWords >>= (genRestLine wordMap 7 "")


-- | Generate 16 bars of straight fire from an array of input lyrics
genBars :: Array String -> RandomEff String
genBars lines =
  let
    splitLines = map (Data.String.split " ") lines
    firstWords = L.toList $ Data.Array.mapMaybe Data.Array.head splitLines
    wordMap = toWordMap splitLines
  in
    do
      liftM1 (Str.joinWith "\n") $
        Data.Array.replicateM 16 (genLine wordMap firstWords)
