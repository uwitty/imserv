{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module View ( HTMLLucid
            , View.list
            , View.ImageListPage
            ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS (unpack)
import           Data.List              (isInfixOf, sort)
import qualified Data.Text              as Text
import qualified Data.Time              as Time
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import           Lucid
import           Network.HTTP.Media     ((//), (/:))
import           Network.URI.Encode
import           Servant                (Accept (..), Handler,
                                         MimeRender (mimeRender))
import           Servant                (Handler, err403, err404, throwError)
import           System.Directory       (listDirectory)
import           System.FilePath        (takeBaseName, takeFileName, (</>))

import qualified API
import qualified API.Data.ImageInfo     as API

data HTMLLucid

instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")


instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml


instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS


data ImageListPage = ImageListPage { title :: String
                                 , entries :: [ImageListEntry]
                                 } deriving (Show, Eq)

data ImageListEntry = ImageListEntry { category :: String
                                   , org        :: API.ImageInfo
                                   } deriving (Show, Eq)

instance ToHtml ImageListPage where
    toHtml page = do
      doctype_
      html_ $ do
          head_ $ do
              title_ [] "image list"
              link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/mui-0.9.39/css/mui.min.css"]
              script_ [src_ "/static/mui-0.9.39/js/mui.min.js"] Text.empty
          body_ $ do
              header_ [class_ "mui-appbar mui--z1"] $ do
                  div_ [class_ "mui-container"] $ do
                      table_ $ tbody_ $ do
                          tr_ [class_ "mui--appbar-height"] $ do
                              td_ [class_ "mui--text-title"] . toHtml $ title page
              div_ [class_ "mui-panel mui--text-center"] $ do
                  div_ [class_ "mui-container"] $ do
                      foldMap toHtml (entries page)
    toHtmlRaw = toHtml


instance ToHtml ImageListEntry where
    toHtml entry = do
        div_ $ do
            div_ [class_ "mui--text-headline"] title
            div_ [class_ "mui--text-dark-secondary"] . toHtml $ dateString
            div_ [class_ "mui--text-dark-secondary"] . toHtml $ API.id item
            img_ [ src_ img_url ]
            --ul_ $ do
            --    li_ [] . toHtml $ API.id item
            --    li_ [] $ img_ [ src_ img_url ]
        div_ [class_ "mui-divider"] ""
      where
        item = org entry
        title = toHtml $ user ++ API.title item
        user = case API.subcategory item of
                   Just a  -> "[" ++ a ++ "] "
                   Nothing -> ""
        userLink = case API.subcategory item of
                   Just a  -> a ++ "/"
                   Nothing -> ""
        img_name = takeFileName $ API.img_urls item !! 0
        img_url = case API.img_paths item of
                      Just paths -> Text.pack $    "/api/imagep/" ++ (API.category item)
                                                ++ "/" ++ (encode (paths !! 0))
                      Nothing    -> Text.pack $    "/api/image/" ++ (API.category item)
                                                ++ "/" ++ userLink ++ img_name
        dateString = toDateString $ API.timestamp item
        toDateString :: Int -> String
        toDateString ctime = Time.formatTime Time.defaultTimeLocale "%Y/%m/%d %H:%M" . posixSecondsToUTCTime $ realToFrac ctime
    toHtmlRaw = toHtml


list :: FilePath -> String -> Maybe String -> Handler ImageListPage
list workDir category mname = do
    API.checkPath category
    name <- nameHandler
    API.checkPath name
    es <- API.list workDir category name >>= return . map toEntry
    return $ ImageListPage (title name) es
  where
    listDir :: FilePath
    listDir = workDir </> category </> "list"
    nameHandler :: Handler FilePath
    nameHandler = case mname of
                      Just name -> return name
                      _         -> do paths <- liftIO $ fmap sort $ listDirectory listDir
                                      if length paths > 0
                                      then return . takeBaseName $ last paths
                                      else throwError err404
    toEntry :: API.ImageInfo -> ImageListEntry
    toEntry item = ImageListEntry category item
    title :: String -> String
    title name = category ++ " - " ++ name

