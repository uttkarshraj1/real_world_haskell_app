{-# LANGUAGE DeriveGeneric #-}

module Parse (
    parseEstablishments,
    fullExtractEst
) where

import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

renameEstablishmentFields "businessName" = "BusinessName"
renameEstablishmentFields "businessType" = "BusinessType" 
renameEstablishmentFields "addressLine1" = "AddressLine1" 
renameEstablishmentFields "addressLine2" = "AddressLine2"
renameEstablishmentFields "addressLine3" = "AddressLine3" 
renameEstablishmentFields "addressLine4" = "AddressLine4"
renameEstablishmentFields "postCode" = "PostCode"
renameEstablishmentFields "ratingValue" = "RatingValue"
renameEstablishmentFields "ratingDate" = "RatingDate"
renameEstablishmentFields "hygiene_score" = "Hygiene"
renameEstablishmentFields "structural_score" = "Structural"
renameEstablishmentFields "confidenceInManagement_score" = "ConfidenceInManagement"
renameEstablishmentFields "fhrsID" = "FHRSID"
renameEstablishmentFields other = other

renameEstablishmentsFields :: [Char] -> [Char]
renameEstablishmentsFields "fHRSEstablishment" = "FHRSEstablishment"
renameEstablishmentsFields other = other

renameEstablishmentCollectionFields :: [Char] -> [Char]
renameEstablishmentCollectionFields "establishmentCollection" = "EstablishmentCollection"
renameEstablishmentCollectionFields other = other

renameEstablishmentDetailFields :: [Char] -> [Char]
renameEstablishmentDetailFields "establishmentDetail" = "EstablishmentDetail"
renameEstablishmentDetailFields other = other

customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameEstablishmentFields
}

establishmentsCustomOptions :: Options
establishmentsCustomOptions = defaultOptions {
    fieldLabelModifier = renameEstablishmentsFields
}

establishmentCollectionCustomOptions :: Options
establishmentCollectionCustomOptions = defaultOptions {
    fieldLabelModifier  =  renameEstablishmentCollectionFields
}

establishmentDetailCustomOptions :: Options
establishmentDetailCustomOptions = defaultOptions {
    fieldLabelModifier  = renameEstablishmentDetailFields
}

instance FromJSON Establishment where
    parseJSON = genericParseJSON customOptions

instance FromJSON Establishments where
    parseJSON = genericParseJSON establishmentsCustomOptions

instance FromJSON EstablishmentCollection where
    parseJSON = genericParseJSON establishmentCollectionCustomOptions

instance FromJSON EstablishmentDetail where
    parseJSON  = genericParseJSON establishmentDetailCustomOptions

parseEstablishments :: L8.ByteString -> Either String Establishments
parseEstablishments json = eitherDecode json :: Either String Establishments

-- parseEstablishmentCollection :: Establishments -> Either String EstablishmentCollection
-- parseEstablishmentCollection ests = eitherDecode ests :: Either String EstablishmentCollection

-- parseEstablishmentDetail :: EstablishmentCollection -> Either String EstablishmentDetail
-- parseEstablishmentDetail ests = eitherDecode ests :: Either String EstablishmentDetail

-- extraction from json
extractCollection :: Establishments -> EstablishmentCollection
extractCollection (Establishments collection) = collection
extractDetail :: EstablishmentCollection -> EstablishmentDetail
extractDetail (EstablishmentCollection detail) = detail
extractEst :: EstablishmentDetail -> [Establishment]
extractEst (EstablishmentDetail ests) = ests

fullExtractEst :: Establishments -> [Establishment]
fullExtractEst establishments = extractEst (extractDetail (extractCollection establishments))

-- fullExtractEst :: Establishments -> EstablishmentDetail
-- fullExtractEst establishments = extractDetail (extractCollection establishments)
