{-# OPTIONS_GHC  -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ImageServerSpec (main, spec) where

import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Client    (Manager, defaultManagerSettings,
                                         newManager)
import           Test.Hspec
import           Test.Hspec.Wai         (get, matchHeaders, shouldRespondWith,
                                         with, (<:>))
import           Test.Hspec.Wai.JSON    (json)

import qualified API.Data.ImageInfo     as API
import qualified ImageServer


main :: IO ()
main = hspec spec

spec :: Spec
spec = imageServerSpec

imageServerSpec :: Spec
imageServerSpec = with (return $ ImageServer.app "testdata") $ do
    describe "/api/list" $ do
        it "GET /api" $ do
            get "/api" `shouldRespondWith` [json| [ "category0", "category1" ] |]
    describe "/api/list/<<category>>" $ do
        it "GET /api/list/category0" $ do
            get "/api/list/category0" `shouldRespondWith`
                [json| [ "2018-05-05"
                       , "2018-05-06"
                       ]
                     |]
        it "returns error when invalid category specified (/...)" $ do
            get "/api/list/%2fcategory0" `shouldRespondWith` 403
        it "returns error when invalid category specified (..)" $ do
            get "/api/list/%2e%2e%2fcategory0" `shouldRespondWith` 403
    describe "/api/list/<<category>>/<<name>>" $ do
        it "GET /api/list/category0/2018-05-05" $ do
            get "/api/list/category0/2018-05-05" `shouldRespondWith`
                [json|[ { category: "category0"
                        , subcategory: "subcategory0"
                        , id: "image001"
                        , timestamp: 1525489200
                        , title: "test images"
                        , img_urls: [ "http://example.com/test.jpg", "http://example.com/test.png"]
                        , img_paths: null
                        , desc: null
                        }
                      ]|]
        it "GET /api/list/category0/2018-05-06" $ do
            get "/api/list/category0/2018-05-06" `shouldRespondWith`
                [json|[ { category: "category0"
                        , subcategory: "jpeg-home"
                        , id: "jpeg001"
                        , timestamp: 1525587556
                        , title: "jpeg home image"
                        , img_urls: ["https://jpeg.org/images/jpeg-home.jpg"]
                        , img_paths: null
                        , desc: null
                        }
                      , { category: "category0"
                        , subcategory: "png-home"
                        , id: "png001"
                        , timestamp: 1525587856
                        , title: "png home image"
                        , img_urls: ["http://www.libpng.org/pub/png/PngSuite/tbbn1g04.png"]
                        , img_paths: null
                        , desc: null
                        }
                      ]|]
        it "GET /api/list/category1" $ do
            get "/api/list/category1" `shouldRespondWith`
                [json| [ "2018-06-10" ] |]
        it "GET /api/list/category1/2018-06-10" $ do
            get "/api/list/category1/2018-06-10" `shouldRespondWith`
                [json|[ { category: "category1"
                        , subcategory: null
                        , id: "image001"
                        , timestamp: 1528601504
                        , title: "test images"
                        , img_urls: [ "http://example.com/file.png"]
                        , img_paths: [ "path/to/file.png"]
                        , desc: null
                        }
                      ]|]
        it "returns error when specified invalid args (1)" $ do
            get "/api/list/%2fcategory1/2018-06-10" `shouldRespondWith` 403
        it "returns error when specified invalid args (2)" $ do
            get "/api/list/%2e%2e%2fcategory1/2018-06-10" `shouldRespondWith` 403
        it "returns error when specified invalid args (3)" $ do
            get "/api/list/category1/%2f2018-06-10" `shouldRespondWith` 403
        it "returns error when specified invalid args (4)" $ do
            get "/api/list/category1/%2e%2e%2f2018-06-10" `shouldRespondWith` 403
    describe "GET /api/image" $ do
        it "/api/image/category0" $ do
            get "/api/image/category0" `shouldRespondWith`
                [json| ["jpeg-home", "png-home", "subcategory0"] |]
        it "returns error when specified invalid args (1)" $ do
            get "/api/image/%2fcategory0" `shouldRespondWith` 403
        it "returns error when specified invalid args (2)" $ do
            get "/api/image/%2e%2e%2fcategory0" `shouldRespondWith` 403
    describe "GET /api/image/<<category>>" $ do
        it "/api/image/category0/subcategory0" $ do
            get "/api/image/category0/subcategory0" `shouldRespondWith`
                [json| ["test.jpg", "test.png"] |]
        it "returns error when specified invalid args (1)" $ do
            get "/api/image/%2fcategory0/subcategory0" `shouldRespondWith` 403
        it "returns error when specified invalid args (2)" $ do
            get "/api/image/%2e%2e%2fcategory0/subcategory0" `shouldRespondWith` 403
        it "returns error when specified invalid args (3)" $ do
            get "/api/image/category0/%2fsubcategory0" `shouldRespondWith` 403
        it "returns error when specified invalid args (4)" $ do
            get "/api/image/category0/%2e%2e%2fsubcategory0" `shouldRespondWith` 403
    describe "GET /api/image/category0/<<subcategory>>/<<image>>" $ do
        it "responds jpeg image: /api/image/category0/jpeg-home/jpeg-home.jpg" $ do
            get "/api/image/category0/jpeg-home/jpeg-home.jpg" `shouldRespondWith`
                200 {matchHeaders = ["Content-Type" <:> "image/jpeg"]}
        it "responds png image: /api/image/category0/png-home/tbbn1g04.png" $ do
            get "/api/image/category0/png-home/tbbn1g04.png" `shouldRespondWith`
                200 {matchHeaders = ["Content-Type" <:> "image/png"]}
        it "returns error when invlaid category specified (/...)" $ do
            get "/api/image/%2fcategory0/jpeg-home/jpeg-home.jpg" `shouldRespondWith` 403
        it "returns error when invlaid category specified (..)" $ do
            get "/api/image/%2e%2e%2fcategory0/jpeg-home/jpeg-home.jpg" `shouldRespondWith` 403
        it "returns error when invlaid subcategory specified (/...)" $ do
            get "/api/image/category0/%2fjpeg-home/jpeg-home.jpg" `shouldRespondWith` 403
        it "returns error when invlaid subcategory specified (..)" $ do
            get "/api/image/category0/%2e%2e%2fjpeg-home/jpeg-home.jpg" `shouldRespondWith` 403
        it "returns error when invlaid name specified (/...)" $ do
            get "/api/image/category0/jpeg-home/%2fjpeg-home.jpg" `shouldRespondWith` 403
        it "returns error when invlaid name specified (..)" $ do
            get "/api/image/category0/jpeg-home/foo%2f%2e%2e%2fjpeg-home.jpg" `shouldRespondWith` 403
    describe "GET /api/imagep" $ do
        it "responds image with path: /api/imagep/category1/path%2fto%2ffile.png" $ do
            get "/api/imagep/category1/path%2fto%2ffile.png" `shouldRespondWith`
                200 {matchHeaders = ["Content-Type" <:> "image/png"]}
        it "returns error when invalid absolute path specified" $ do
            get "/api/imagep/category1/%2fpath%2fto%2ffile.png" `shouldRespondWith` 403
        it "returns error when invalid path specified (..)" $ do
            get "/api/imagep/category1/%2e%2e%2fpath%2fto%2ffile.png" `shouldRespondWith` 403
        it "returns error when invalid category specified (/...)" $ do
            get "/api/imagep/%2fvar%2f/list%test" `shouldRespondWith` 403
        it "returns error when invalid category specified (..)" $ do
            get "/api/imagep/%2e%2e%2f%2e%2e%2fdata%2fsomeone/list%test" `shouldRespondWith` 403
    describe "GET /static" $ do
        it "/static/test.txt" $ do
            get "/static/test.txt" `shouldRespondWith` 200
