{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Options.Declarative

import qualified ImageServer

imserv :: Flag "" '["working-dir"] "PATH" "working directory" (Def "." String)
       -> Cmd "imserv command" ()
imserv workingDir = liftIO . ImageServer.run $ get workingDir

main :: IO ()
--main = ImageServer.run "testdata"
main = run_ imserv

