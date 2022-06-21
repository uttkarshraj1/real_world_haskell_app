{-# LANGUAGE DeriveGeneric #-}

module Types (
    Establishment (..),
    Establishments (..),
    EstablishmentCollection (..),
    EstablishmentDetail (..),
    FetchURL (..),
    DataInsert (..)
) where

import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Types
import GHC.Generics

data FetchURL = FetchURL {
    id_ :: Int,
    url :: String
} deriving (Show, Generic)

-- declaration of the main type that we are working with
data Establishment = Establishment {
    fhrsID :: String,
    businessName :: String,
    businessType :: String,
    postCode :: Maybe String,
    ratingValue :: Maybe String,
    ratingDate :: Maybe String,
    addressLine1 :: Maybe String,
    addressLine2 :: Maybe String,
    addressLine3 :: Maybe String,
    addressLine4 :: Maybe String
} deriving (Show, Generic)

data DataInsert = DataInsert {
    fhrs_id :: String,
    business_name :: String,
    business_type :: String,
    post_code :: Maybe String,
    rating_value :: Maybe String,
    rating_date :: Maybe String,
    address_line1 :: Maybe String,
    address_line2 :: Maybe String,
    address_line3 :: Maybe String,
    address_line4 :: Maybe String,
    fk_fetchUrl :: Int
} deriving (Show, Generic)

-- the json is strustured so that EstablishmentCollection data lies within Establishments
data Establishments = Establishments {
    fHRSEstablishment :: EstablishmentCollection
} deriving (Show, Generic)

-- the json is strustured so that EstablishmentDetail data lies within EstablishmentCollection
data EstablishmentCollection = EstablishmentCollection {
    establishmentCollection :: EstablishmentDetail
} deriving (Show, Generic)

-- the json is strustured so that Establishment data lies within EstablishmentDetail
data EstablishmentDetail = EstablishmentDetail {
    establishmentDetail :: [Establishment]
} deriving (Show, Generic)

-- | construct ToRow instance with 11 ToField
instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k) where
    toRow (a,b,c,d,e,f,g,h,i,j,k) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k]
