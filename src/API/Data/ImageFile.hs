{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module API.Data.ImageFile
    ( ImageFileResponse (..)
    , ImageFile
    ) where

import qualified Data.ByteString.Lazy     as BS
import           Network.HTTP.Media       ((//))
import           Servant                  (Accept (..))
import           Servant.API.ContentTypes (AllCTRender (..))


data ImageFile

instance Accept ImageFile where
    contentType _ = "image" // "jpeg"

data ImageFileResponse = ImageFileResponse { header :: BS.ByteString, content :: BS.ByteString }

instance AllCTRender '[ImageFile] ImageFileResponse where
    handleAcceptH _ _ (ImageFileResponse h c) = Just (h, c)

