{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    saveEstablishments,
    checkOrAddUrl
) where

import Types
import FetchHTTP
import Parse
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

instance FromRow Establishment where
    fromRow = Establishment <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DataInsert where
    toRow (DataInsert fhrs_id business_name business_type post_code rating_value rating_date addressLine1 address_line2 address_line3 address_line4 fk_fetchUrl)
        = toRow (fhrs_id, business_name, business_type, post_code, rating_value, rating_date, addressLine1, address_line2, address_line3, address_line4, fk_fetchUrl)

instance FromRow FetchURL where
    fromRow = FetchURL <$> field <*> field

instance ToRow FetchURL where
    toRow (FetchURL id_ url)
        = toRow (id_, url)


-- | initial Database Connection
initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "ratings.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS establishments (\
            \fhrsID VARCHAR(50) PRIMARY KEY,\
            \businessName VARCHAR(200) NOT NULL, \
            \businessType VARCHAR(50) NOT NULL, \
            \postCode VARCHAR(10) DEFAULT NULL, \
            \ratingValue VARCHAR(2) DEFAULT NULL, \
            \ratingDate VARCHAR(50) DEFAULT NULL, \
            \addressLine1 VARCHAR(200) DEFAULT NULL, \
            \addressLine2 VARCHAR(200) DEFAULT NULL, \
            \addressLine3 VARCHAR(200) DEFAULT NULL, \
            \addressLine4 VARCHAR(200) DEFAULT NULL, \
            \fk_fetchUrl INTEGER\
            \)"        
        execute_ conn "CREATE TABLE IF NOT EXISTS fetchURLs (\
            \id_ INTEGER PRIMARY KEY AUTOINCREMENT,\
            \url VARCHAR(200) NOT NULL \
            \)"
        return conn

-- | insert new URL into database
createURL :: Connection -> String  -> IO ()
createURL conn url = do
    execute conn "INSERT INTO fetchURLs (url) VALUES (?)" (Only url)

-- | check if this establishment already exist, if true, then change it to the current url id
checkIfEstablishmentExists :: Connection -> String -> IO [Establishment]
checkIfEstablishmentExists conn fhrsID = do
    result <- queryNamed conn "SELECT fhrsID FROM establishments WHERE fhrsID=:id" [":id" := fhrsID]
    return result

-- | insert parsed establish into database 
createEstablishment :: Connection -> Int -> Establishment -> IO ()
createEstablishment conn urlId establishment = do
    result <- checkIfEstablishmentExists conn (fhrsID establishment)
    if length result > 0
        then do
            putStrLn "Establishment exists in db"
            execute conn "UPDATE establishments SET fk_fetchUrl = '?' WHERE fhrsID = '?';" (urlId, (fhrsID establishment))
            -- queryNamed conn "UPDATE establishments SET fk_fetchUrl = :fetchUrl  WHERE fhrsID = :id" [":fetchUrl" := url, ":id" := (fhrsID establishment)]
    else do
        let est = DataInsert {
            fhrs_id = fhrsID establishment,
            business_name = businessName establishment,
            business_type = businessType establishment,
            post_code = postCode establishment,
            rating_value = ratingValue establishment,
            rating_date = ratingDate establishment,
            address_line1 = addressLine1 establishment,
            address_line2 = addressLine2 establishment,
            address_line3 = addressLine3 establishment,
            address_line4 = addressLine4 establishment,
            fk_fetchUrl = urlId
        }
        execute conn "INSERT INTO establishments VALUES (?,?,?,?,?,?,?,?,?,?,?)" est

saveEstablishments :: Connection -> Int -> [Establishment] -> IO ()
saveEstablishments conn urlId = mapM_ (createEstablishment conn urlId)

-- | check whether the data exist in database or not
lookupURL :: Connection -> String -> IO [FetchURL]
lookupURL conn url = do
    results <- queryNamed conn "SELECT * FROM fetchURLs WHERE url=:url" [":url" := url]
    return results

-- | pass all the data to corresponding function
addUrlAndEstablishments :: Connection  -> String -> IO ()
addUrlAndEstablishments conn url = do
    establishmentsByteString <- downloadURL url
    case (parseEstablishments establishmentsByteString) of
        Left err -> print err
        Right ests -> do
            createURL conn url
            resultURL <- lookupURL conn url
            saveEstablishments conn (id_ (resultURL !! 0)) (fullExtractEst ests)

-- | function for main, return query result
checkOrAddUrl :: Connection -> String -> IO [Establishment]
checkOrAddUrl conn url = do
    resultURL <- lookupURL conn url
    if length resultURL > 0
        then do
            putStrLn "Data Exists in Database"
            result <- queryNamed conn "SELECT fhrsID,businessName,businessType,postCode,ratingValue,ratingDate,addressLine1,addressLine2,addressLine3,addressLine4 FROM establishments WHERE fk_fetchUrl=:id" [":id" := (id_ (resultURL !! 0))]
            return result
    else do
        putStrLn "Downloading data"
        addUrlAndEstablishments conn url
        checkOrAddUrl conn url
