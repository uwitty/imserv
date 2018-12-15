{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module ImageServer
    ( run
    , app
    ) where

import qualified Data.ByteString.Lazy     as BS
import           Data.Proxy               (Proxy (..))
import           Network.HTTP.Media       ((//))
import qualified Network.Wai.Handler.Warp as Warp (run)
import           Servant                  ((:<|>) (..), (:>), Application,
                                           Capture, Get, JSON, QueryParam, Raw,
                                           Server, serve, serveDirectoryWebApp)
import           System.FilePath          ((</>))

import qualified API
import           API.Data.ImageFile       (ImageFile, ImageFileResponse)
import qualified API.Data.ImageInfo       as API
import qualified View

type API = "api" :> (    Get '[JSON] [String]
                    :<|> "list" :> Capture "category" String :>
                         (    Get '[JSON] [String]
                         :<|> Capture "name" String :> Get '[JSON] [API.ImageInfo]
                         )
                    :<|> "image" :> Capture "category" String :>
                         (    Get '[JSON] [String]
                         :<|> Capture "subcategory" String :> Get '[JSON] [String]
                         :<|> Capture "subcategory" String :> Capture "name" String :>
                              Get '[ImageFile] ImageFileResponse
                         )
                    :<|> "imagep" :> Capture "category" String :>
                         Capture "path" String :>
                         Get '[ImageFile] ImageFileResponse
                    )
      :<|> "static" :> Raw
      :<|> "list" :> Capture "category" String :> QueryParam "name" String
           :> Get '[View.HTMLLucid] View.ImageListPage

server :: FilePath -> Server API
server workDir = (    API.categories workDir
                 :<|> listOperations
                 :<|> imageOperations
                 :<|> API.imageFilePath workDir
                 )
                 :<|> serveStatic
                 :<|> View.list workDir
  where
    listOperations category  =    API.lists workDir category
                             :<|> API.list  workDir category
    imageOperations category =    API.imageSubcategories workDir category
                             :<|> API.imageNames workDir category
                             :<|> API.imageFile  workDir category
    serveStatic             = serveDirectoryWebApp $ "static"

userAPI :: Proxy API
userAPI = Proxy

app :: FilePath -> Application
app = serve userAPI . server

run :: FilePath -> IO ()
run = Warp.run 8081 . app

