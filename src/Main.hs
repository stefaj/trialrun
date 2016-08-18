{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Process
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import System.Directory

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (serveFile "web/index.html") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("proc", processHandler)
          ] <|>
    serveDirectory "web"
    -- dir "web" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param


processHandler :: Snap ()
processHandler = do
    B.pack <$> (liftIO getCurrentDirectory) >>= writeBS
    (o,e) <- liftIO $ startProcess "./test.sh" []
    writeBS (B.pack o)

