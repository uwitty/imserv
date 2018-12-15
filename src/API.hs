{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module API
    ( API.categories
    , API.lists
    , API.list
    , API.imageSubcategories
    , API.imageNames
    , API.imageFile
    , API.imageFilePath
    , checkPath
    ) where

import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (decode)
import qualified Data.ByteString.Lazy   as BS
import           Data.Char              (toLower)
import           Data.List              (isInfixOf, sort)
import           Servant                ((:<|>) (..), Handler, err403, err404,
                                         throwError)
import           System.Directory       (doesDirectoryExist, listDirectory)
import           System.Exit            (exitSuccess)
import           System.FilePath        (takeBaseName, takeExtension, (<.>),
                                         (</>))

import qualified API.Data.ImageFile     as ImageFile
import qualified API.Data.ImageInfo     as API

isValidArgument :: String -> Bool
isValidArgument arg
  | length arg == 0    = True
  | head arg == '/'    = False
  | isInfixOf ".." arg = False
  | otherwise          = True


checkPath :: String -> Handler String
checkPath path
  | isValidArgument path = return path
  | otherwise            = throwError err403


categories :: FilePath -> Handler [String]
categories workDir = liftIO $ getCategories
  where
    getCategories :: IO [FilePath]
    getCategories = listDirectory workDir >>=
                    return . filter (/= "static") >>=
                    filterM isDir >>=
                    return . sort
    isDir :: FilePath -> IO Bool
    isDir = doesDirectoryExist . (workDir </>)


lists :: FilePath -> String -> Handler [String]
lists workDir category = do
    checkPath category
    names <- liftIO $ listDirectory listDir
    return . sort . map takeBaseName $ names
  where
    listDir :: FilePath
    listDir = workDir </> category </> "list"


list :: FilePath -> String -> String -> Handler [API.ImageInfo]
list workDir category name = do
    checkPath category
    checkPath name
    content <- return listFileName >>= liftIO . BS.readFile
    liftIO $ print listFileName
    case decode content of
        Just lists -> return lists
        Nothing   -> do liftIO $ putStrLn $ "failed to decode: " ++ listFileName
                        throwError err404
  where
    listFileName :: FilePath
    listFileName = workDir </> category </> "list" </> name <.> "json"


imageSubcategories :: FilePath -> String -> Handler [String]
imageSubcategories workDir category = do
    checkPath category
    imageSubcategories <- liftIO $ listDirectory dataDir
    return . sort $ imageSubcategories
  where
    dataDir = workDir </> category </> "data"


imageNames :: FilePath -> String -> String -> Handler [String]
imageNames workDir category subcategory = do
    checkPath category
    checkPath subcategory
    imageNames <- liftIO $ listDirectory imageDir
    return . sort $ imageNames
  where
    imageDir = workDir </> category </> "data" </> subcategory </> "images"


imageFile :: FilePath -> String -> String -> String -> Handler ImageFile.ImageFileResponse
imageFile workDir category subcategory name = do
    checkPath category
    checkPath subcategory
    checkPath name
    respondImageFile fpath
  where
    fpath :: FilePath
    fpath = workDir </> category </> "data" </> subcategory </> "images" </> name


imageFilePath :: FilePath
              -> String
              -> String
              -> Handler ImageFile.ImageFileResponse
imageFilePath workDir category path = do
  checkPath category
  checkPath path
  respondImageFile $ workDir </> category </> "data" </> path


respondImageFile :: FilePath -> Handler ImageFile.ImageFileResponse
respondImageFile fpath = do
    content <- liftIO $ BS.readFile fpath
    return $ ImageFile.ImageFileResponse ctype content
  where
    ctype :: BS.ByteString
    ctype = case (fmap toLower . takeExtension) fpath of
                ".jpeg" -> "image/jpeg"
                ".jpg"  -> "image/jpeg"
                ".png"  -> "image/png"
                ".gif"  -> "image/gif"
                ".bmp"  -> "image/bmp"
                ".webp" -> "image/webp"
                _       -> "image"
