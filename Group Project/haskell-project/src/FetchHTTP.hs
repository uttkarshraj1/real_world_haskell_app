module FetchHTTP where

import Network.HTTP.Simple
import Control.Exception
import Data.ByteString.Lazy.Char8 as C
import Data.List as L

-- fetching the html page behind a url
downloadURL :: String -> IO ByteString
downloadURL url = do
    request <- try (parseRequest url) :: IO (Either SomeException Request)
    case request of 
        Left requestEx  -> return $ C.pack $ "Could not parse the request due to: " ++ show requestEx
        Right requestVal -> do
            response <- try (httpLBS requestVal) :: IO (Either SomeException (Response ByteString))
            case response of
                Left responseEx  -> return $ C.pack $ "Could not get response due to: " ++ show responseEx
                Right responseVal -> return $ getResponseBody responseVal

-- constructing requests

-- top 10 places in an area
getTopStatsAreaLink :: String -> String
getTopStatsAreaLink area = "https://ratings.food.gov.uk/enhanced-search/en-GB/^/" ++ area ++ "/rating/0/^/1/10/json"

-- top 10 places of a type
getTopStatsTypeLink :: String -> String
getTopStatsTypeLink businessType = "https://ratings.food.gov.uk/enhanced-search/en-GB/^/^/rating/" ++ businessType ++ "/^/1/10/json"

-- top 10 places of a type in an area
getTopStatsAreaTypeLink :: String -> String -> String
getTopStatsAreaTypeLink area businessType = "https://ratings.food.gov.uk/enhanced-search/en-GB/^/" ++ area ++ "/rating/" ++ businessType ++ "/^/1/10/json"

-- bottom 10 places in an area
getBottomStatsAreaLink :: String -> String
getBottomStatsAreaLink area = "https://ratings.food.gov.uk/enhanced-search/en-GB/^/" ++ area ++ "/desc_rating/0/^/1/10/json"

-- bottom 10 places of a type
getBottomStatsTypeLink :: String -> String
getBottomStatsTypeLink businessType = "https://ratings.food.gov.uk/enhanced-search/en-GB/^/^/desc_rating/" ++ businessType ++ "/^/1/10/json"

-- bottom 10 places of a type in an area
getBottomStatsAreaTypeLink :: String -> String -> String
getBottomStatsAreaTypeLink area businessType = "https://ratings.food.gov.uk/enhanced-search/en-GB/^/" ++ area ++ "/desc_rating/" ++ businessType ++ "/^/1/10/json"

-- top 10 places with this adress
getInfoAdressLink :: String -> String
getInfoAdressLink adress = "https://ratings.food.gov.uk/enhanced-search/en-GB/^/" ++ adress ++ "/rating/0/^/1/10/json"

-- top 10 places with this name
getInfoNameLink :: String -> String
getInfoNameLink name = "https://ratings.food.gov.uk/enhanced-search/en-GB/" ++ name ++ "/^/rating/0/^/1/10/json"

-- catching errors in input
inputError :: String -> Bool
inputError i = i == "Error: wrong input"

-- catching errors in page
pageError :: C.ByteString -> Bool
pageError p = 
      let { page = C.unpack p;
            nonEmptySearch = "EstablishmentCollectionEstablishmentDetailFHRSIDLocalAuthorityBusinessIDBusinessNameBusinessTypeBusinessTypeIDAddressLine1AddressLine2AddressLine3AddressLine4PostCode" }
            in not (L.isSubsequenceOf nonEmptySearch page)
            
-- catching pages with only one establishment
onePageError :: C.ByteString -> C.ByteString
onePageError p = 
      let { page = C.unpack p;
            tailPage = Prelude.last (Prelude.take 20 (Prelude.iterate (Prelude.tail.(Prelude.dropWhile (/= ':'))) page));
            initPage = Prelude.take (Prelude.length page - Prelude.length tailPage) page;
            needArray = Prelude.head tailPage /= '[';
            }
            in if (Prelude.head tailPage /= '[') then C.pack (initPage ++ "[" ++ (Prelude.take (Prelude.length tailPage - 3) tailPage) ++ "]}}}") else p            
