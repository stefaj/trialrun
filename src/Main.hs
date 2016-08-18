{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Process
import Control.Monad.Trans
import System.Directory
import Data.Monoid

import Prelude hiding (writeFile)


import qualified Crypto.Hash.MD5 as Crypto
import           Data.ByteString (ByteString, writeFile,append)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (serveFile "web/index.html") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("compile", processHandler)
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
    Just compiler_ <- getParam "compiler" 
    let compiler = getCompiler compiler_
    Just source <- getParam "source"

    case compiler of
      Just GCC -> liftIO $ runCompiler GCC source >> return ()
      Just GHC -> liftIO $ runCompiler GHC source >> return ()
      Nothing -> do
             BC.pack <$> (liftIO getCurrentDirectory) >>= writeBS
             (o,e) <- liftIO $ startProcess "./test.sh" []
             writeBS (BC.pack o)

data Compiler = GCC
              | GHC


getCompiler "gcc" = Just GCC
getCompiler "ghc" = Just GHC
getCompiler _ = Nothing

getCompilerExec :: Compiler -> (String, [String]) -- (execName,args)
getCompilerExec GCC = ("gcc",[])
getCompilerExec GHC = ("ghc",[])

type Code = ByteString

data CompileResult = CompileResult {cr_time :: Float, cr_errors :: String, cr_out :: String}

runCompiler :: Compiler -> Code -> IO CompileResult
runCompiler comp code = do
  let (exec, args)  = getCompilerExec comp
  let hash = getHash code 
  let filename = ("data/" <> hash)
  writeFile (BC.unpack filename) code

  (o,e) <- startProcess exec args
  return $ CompileResult 0 o e

getHash :: ByteString -> ByteString
getHash = BC.takeWhile (/= '=') . BC.map toWebSafe . B64.encode . Crypto.hash
  where toWebSafe '/' = '_'
        toWebSafe '+' = '-'
        toWebSafe c   = c




