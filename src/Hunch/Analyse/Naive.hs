module Hunch.Analyse.Naive 
       ( identifyLanguage
       , identifyLanguage'  
       ) where

import           Data.Function
import           Data.List
import qualified Data.Set           as Set
import           Data.String.Utils
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE 
import           Safe

import Hunch.Types

--------------------------------------------------------------------------------

-- | check if Basic Latin
isLatin   :: Char -> Bool
isLatin c = c >= '\x0000' && c <= '\x007F'

isCyrillic   :: Char -> Bool
isCyrillic c = c >= '\x0400' && c <= '\x04FF'

isHebrew   :: Char -> Bool
isHebrew c = c >= '\x0590' && c <= '\x05FF'

-- English  Range: 0000–007F
-- Cyrillic Range: 0400–04FF
-- Hebrew   Range: 0590–05FF

checkLang :: Char -> Writing
checkLang char
  | isLatin    char = WLatin
  | isHebrew   char = WHebrew
  | isCyrillic char = WCyrillic
  | otherwise       = WOther
                                
identifyLanguage :: String -> String
identifyLanguage str = do
  let res   = map checkLang $ replace " " "" $ strip str
      res'  = rmdups res
      res'' = intercalate " - " $ map show res'
      occ   = reverse $ sortBy (compare `on` snd) $ occurs res
  let resO  = case length occ of
                0         -> ""
                1         -> show $ fst $ occ!!0
                otherwise -> intercalate "-" $ map (show . fst) [occ!!0, occ!!1]
  resO

identifyLanguage' :: String -> Writing
identifyLanguage' str = do
  let res   = map checkLang $ replace " " "" $ strip str
      res'  = rmdups res
  case headMay res' of
    Nothing  -> WOther
    Just l   -> l

-- |
-- 
rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c    

-- | List of elements occurences inside
--   of list
occurs :: Eq a => [a] -> [(a, Int)]
occurs xs =
  occursAux [] xs 
    where
      occursAux :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
      occursAux init    []  = init 
      occursAux init (x:xs) = occursAux (initNew init x) xs
        where

          initNew :: Eq a => [(a, Int)] -> a -> [(a, Int)]
          initNew init x = (remove' init x) ++ [(eval init x)] 

          eval :: Eq a => [(a, Int)] -> a -> (a, Int)
          eval []   x = (x, 1)
          eval init x =
            case lookup x init of
              Nothing  -> (x, 1)
              Just val -> (x, val+1)

          remove' :: Eq a => [(a, Int)] -> a -> [(a, Int)]
          remove' [] _  = []
          remove' xs el = filter (\x@(a,i) -> a /= el) xs 
