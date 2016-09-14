module Naive where
       ( identifyLanguage
       , identifyLanguage'  
       )

--------------------------------------------------------------------------------

isCyrillic   :: Char -> Bool
isCyrillic c = c >= '\x0400' && c <= '\x04FF'

-- | check if Basic Latin
isLatin   :: Char -> Bool
isLatin c = c >= '\x0000' && c <= '\x007F'

isHebrew   :: Char -> Bool
isHebrew c = c >= '\x0590' && c <= '\x05FF'

-- English  Range: 0000–007F
-- Cyrillic Range: 0400–04FF
-- Hebrew   Range: 0590–05FF

checkLang :: Char -> Lang
checkLang char
  | isLatin    char = Latin
  | isHebrew   char = Hebrew
  | isCyrillic char = Cyrillic
  | otherwise       = Other
                                
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

identifyLanguage' :: String -> Lang
identifyLanguage' str = do
  let res   = map checkLang $ replace " " "" $ strip str
      res'  = rmdups res
  case headMay res' of
    Nothing  -> Other
    Just l   -> l

-- |
-- 
rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c    
