{-# LANGUAGE DeriveGeneric #-}

module API.Data.ImageInfo
    ( ImageInfo (..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

data ImageInfo = ImageInfo { category    :: String
                           , subcategory :: Maybe String
                           , id          :: String
                           , timestamp   :: Int
                           , title       :: String
                           , img_urls    :: [FilePath]
                           , img_paths   :: Maybe [FilePath]
                           , desc        :: Maybe String
                           } deriving (Show, Generic, Eq)

instance ToJSON ImageInfo
instance FromJSON ImageInfo

