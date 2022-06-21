module InputNormalisation where

import qualified Data.Text as T
import Data.Char as Ch

-- replacing spaces with %20
spacesNormalisation :: [String] -> String
spacesNormalisation [] = ""
spacesNormalisation [w] = w
spacesNormalisation (w:ws) = w ++ "%20" ++ (spacesNormalisation ws)

-- punctuation normalization
punctuationNormalisation :: String -> String 
punctuationNormalisation [] = ""
punctuationNormalisation (l:ls) = if Ch.isPunctuation l then "" ++ punctuationNormalisation ls
                                  else l : punctuationNormalisation ls

-- split to words and lowercase line 
lineNormalisation :: String -> String 
lineNormalisation line = spacesNormalisation $ words $ punctuationNormalisation (T.unpack (T.toLower (T.pack line)))

-- making numbers
numberNormalisation :: String -> String
numberNormalisation n = case n of
                             "1" -> "7"
                             "2" -> "7838"
                             "3" -> "5"
                             "4" -> "7842"
                             "5" -> "14"
                             "6" -> "7839"
                             "7" -> "7846"
                             "8" -> "7841"
                             "9" -> "7843"
                             "10" -> "1"
                             "11" -> "4613"
                             "12" -> "7840"
                             "13" -> "7845"
                             "14" -> "7844"
                             otherwise -> "Error: wrong input"
