module ShowResult where

import Types
import Data.Maybe

dispalyResult :: [Establishment] -> IO ()
dispalyResult resultData = do
    putStrLn "----------------------------------------------------------------------------------------------------------------"
    putStrLn listHead
    mapM_ (putStrLn . resultToString) resultData
    putStrLn "----------------------------------------------------------------------------------------------------------------"

listHead :: String
listHead = " || "
    ++ "Business Name" ++ " || "
    ++ "Business Type" ++ " || "
    ++ "Post Code" ++ " || " 
    ++ "Rating Value" ++ " || " 
    ++ "Rating Date" ++ " || " 
    ++ "        address      " ++ " || " 

resultToString :: Establishment -> String
resultToString result = " || "
    ++ (businessName result) ++ " || "
    ++ (businessType result) ++ " || "
    ++ (fromMaybe "" (postCode result)) ++ " || "
    ++ (fromMaybe "" (ratingValue result)) ++ " || "
    ++ (fromMaybe "" (ratingDate result)) ++ " || "
    ++ (fromMaybe "" (addressLine1 result)) ++ ", "
    ++ (fromMaybe "" (addressLine2 result)) ++ ", "
    ++ (fromMaybe "" (addressLine3 result)) ++ ", "
    ++ (fromMaybe "" (addressLine4 result)) ++ " || "