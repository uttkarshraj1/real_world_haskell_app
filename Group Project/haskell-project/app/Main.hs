module Main where
    
import FetchHTTP
import InputNormalisation
import Types
import Types ( Establishment(Establishment) )
import Database
import System.IO
import Parse
import ShowResult
import Database.SQLite.Simple
import System.Exit
import System.Directory

import qualified Data.ByteString.Lazy.Char8 as C

-- basic UI
main :: IO ()
main = do
    putStrLn "---------------------------------"
    putStrLn "  Welcome to the app"
    putStrLn "  (1) Check specific place         "
    putStrLn "  (2) Recommendations     "
    putStrLn "  (3) Quit                       "
    putStrLn "---------------------------------"
    conn <- initialiseDB
    checkOption <- getLine
    case () of
      ()| checkOption == "1" -> do
            putStrLn "---------------------------------"
            putStrLn "  (1) Search by address"
            putStrLn "  (2) Search by name"
            putStrLn "  (3) Back"
            putStrLn "---------------------------------"
            searchType <- getLine
            case () of
                () | searchType == "1" -> do
                       putStrLn "---------------------------------"
                       putStrLn "Please Input the address"
                       putStrLn "Or type back to return"
                       putStrLn "---------------------------------"
                       address <- getLine
                       if address == "back"
                           then main
                       else return()
                       let getDataLink = getInfoAdressLink (lineNormalisation address)
                       result <- checkOrAddUrl conn getDataLink
                       dispalyResult result
                       main
                   | searchType == "2" -> do
                       putStrLn "---------------------------------"
                       putStrLn "Please Input the address"
                       putStrLn "Or type back to return"
                       putStrLn "---------------------------------"
                       name <- getLine
                       if name == "back"
                           then main
                       else return()
                       let getDataLink = getInfoNameLink (lineNormalisation name)
                       result <- checkOrAddUrl conn getDataLink
                       dispalyResult result
                       main
                   | searchType == "3" -> do
                       main
                   | otherwise -> do 
                       print "Invalid option"
                       main
        | checkOption == "2"-> do
            recommend conn
        | checkOption == "3"-> do
            removeFile("ratings.sqlite")
            die("Hope you've enjoyed using the app!")
        |otherwise -> do
            print "Invalid option"
            main

-- part of the code that shows statistics
recommend :: Connection -> IO ()
recommend conn = do
    let sortSchemem = ["  (1) Sort By Type","  (2) Sort By Area","  (3) Sort By Type and Area","  (4) Go Back"]
    let businessType = ["  (1) Distributors/Transporters","  (2) Farmers/growers","  (3) Hospitals/Childcare/Caring Premises","  (4) Hotel/bed & breakfast/guest house","  (5) Importers/Exporters","  (6) Manufacturers/packers","  (7) Mobile caterer","  (8) Other catering premises","  (9) Pub/bar/nightclub","  (10) Restaurant/Cafe/Canteen","  (11) Retailers - other","  (12) Retailers - supermarkets/hypermarkets", "  (13) School/college/university"]
    let sortType = ["  (1) Top 10","  (2) Bottom 10","  (3) Go Back"]
    putStrLn "---------------------------------"
    putStrLn "Please choose the recommandation scheme"
    putStrLn "---------------------------------"
    mapM_ (putStrLn . show) sortSchemem
    putStrLn "---------------------------------"
    sortOption <- getLine
    case () of
      ()| sortOption== "1"-> do
            putStrLn "---------------------------------"
            putStrLn "Please choose the business"
            putStrLn "---------------------------------"
            mapM_ (putStrLn . show) businessType
            putStrLn "---------------------------------"
            chosenBusiness <- getLine
            if (inputError (numberNormalisation chosenBusiness) == True)
                then do
                    putStrLn "Invalid option"
                    recommend conn
            else return()
            putStrLn "---------------------------------" 
            putStrLn "Please choose the result you want"
            putStrLn "---------------------------------"
            mapM_ (putStrLn . show) sortType 
            putStrLn "---------------------------------"
            chosenOption <- getLine
            if chosenOption == "5"
                then recommend conn
            else return()
            case () of
                ()|chosenOption == "1" -> do
                    let getDataLink = getTopStatsTypeLink (numberNormalisation chosenBusiness)
                    result <- checkOrAddUrl conn getDataLink
                    dispalyResult result
                    main
                  |chosenOption == "2" -> do
                    let getDataLink = getBottomStatsTypeLink (numberNormalisation chosenBusiness)
                    result <- checkOrAddUrl conn getDataLink
                    dispalyResult result
                    main
                  |otherwise -> do
                      print "Invalid option"
                      recommend conn        
        | sortOption== "2"-> do
            putStrLn "---------------------------------"
            putStrLn "Please Input your Area"
            putStrLn "Or type back to return"
            putStrLn "---------------------------------"
            area <- getLine
            if area == "back"
                then recommend conn
            else return()
            putStrLn "---------------------------------"
            putStrLn "Please choose the result you want"
            putStrLn "---------------------------------"
            mapM_ (putStrLn . show) sortType
            putStrLn "---------------------------------"
            chosenOption <- getLine
            if chosenOption == "5"
                then recommend conn
            else return()
            case () of
                ()|chosenOption == "1" -> do
                    let getDataLink = getTopStatsAreaLink (lineNormalisation area)
                    result <- checkOrAddUrl conn getDataLink
                    dispalyResult result
                    main
                  |chosenOption == "2" -> do
                    let getDataLink = getBottomStatsAreaLink (lineNormalisation area)
                    result <- checkOrAddUrl conn getDataLink
                    dispalyResult result
                    main
                  |otherwise -> do
                      print "Invalid option"
                      recommend conn
        | sortOption== "3"-> do
            putStrLn "---------------------------------"
            putStrLn "Please choose the business"
            putStrLn "---------------------------------"
            mapM_ (putStrLn . show) businessType
            putStrLn "---------------------------------"
            chosenBusiness <- getLine
            if (inputError (numberNormalisation chosenBusiness) == True)
                then do
                    putStrLn "Invalid option"
                    recommend conn
            else return()
            putStrLn "---------------------------------"
            putStrLn "Please Input your Area"
            putStrLn "Or type back to return"
            putStrLn "---------------------------------"
            area <- getLine
            if area == "back"
                then recommend conn
            else return()
            putStrLn "---------------------------------"
            putStrLn "Please choose the result you want"
            putStrLn "---------------------------------"
            mapM_ (putStrLn . show) sortType
            putStrLn "---------------------------------"
            chosenOption <- getLine
            if chosenOption == "5"
                then recommend conn
            else return()
            case () of
                ()|chosenOption == "1" -> do
                    let getDataLink = getTopStatsAreaTypeLink (lineNormalisation area) (numberNormalisation chosenBusiness)
                    result <- checkOrAddUrl conn getDataLink
                    dispalyResult result
                    main
                  |chosenOption == "2" -> do
                    let getDataLink = getBottomStatsAreaTypeLink (lineNormalisation area) (numberNormalisation chosenBusiness)
                    result <- checkOrAddUrl conn getDataLink
                    dispalyResult result
                    main
                  |otherwise -> do
                      print "Invalid option"
                      recommend conn
        | sortOption== "4"-> do
            main
        |otherwise -> do
            print "Invalid option"
            recommend conn
