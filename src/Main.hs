{-# LANGUAGE ScopedTypeVariables #-}
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
import           Snap.Util.FileServe
import           Snap.Util.FileUploads

main :: IO ()
main = quickHttpServe $ (processBody >> site) <|> site

site :: Snap ()
site =
    ifTop (serveFile "web/index.html") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("compile", processHandler)
          ] <|>
    serveDirectory "web"
    -- dir "web" (serveDirectory ".")



uploadPolicy :: UploadPolicy
uploadPolicy = setMaximumFormInputSize (2^(22 :: Int)) defaultUploadPolicy

processBody :: Snap ()
processBody = do
    handleMultipart uploadPolicy (\x y -> return ())
    return ()

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

    comp <- case compiler of
      Just GCC -> liftIO $ runCompiler GCC source
      Just GHC -> liftIO $ runCompiler GHC source
      Nothing -> return $ Left $ Error "Invalid compiler"

    rr_ <- case comp of
             Left e -> return $ Left e
             Right cr -> liftIO $ runFile (cr_file cr)
    
    case rr_ of
      Left (Error e) -> writeBS $ BC.pack e -- Error
      Right rr -> writeBS $ BC.pack $ rr_result rr



data Compiler = GCC
              | GHC


getCompiler "gcc" = Just GCC
getCompiler "ghc" = Just GHC
getCompiler _ = Nothing

getCompilerArgs :: Compiler -> String -> String -> (String, [String]) -- (execName,args)
getCompilerArgs GCC filename outFile = ("gcc",["-o " <> outFile])
getCompilerArgs GHC filename outFile = ("ghc",[])

getCompilerExt :: Compiler -> String
getCompilerExt GCC = ".cpp"
getCompilerExt GHC = ".hs"


type Code = ByteString

data CompileResult = CompileResult {cr_time :: Float, cr_result :: String, cr_file :: String}

data Error = Error String

data RunResult = RunResult {rr_time :: Float, rr_result :: String}

runFile :: String -> IO (Either Error RunResult )
runFile filename = do
  (o,e) <- startProcess filename []
  return $ Right $ RunResult 0 (o <> e)

runCompiler :: Compiler -> Code -> IO (Either Error CompileResult )
runCompiler comp code = do
  let ext = getCompilerExt comp
  let hash = getHash code 

  let outFile = "data/" <> (BC.unpack hash)

  let filename = outFile <> ext
  writeFile filename code

  let (exec, args)  = getCompilerArgs comp filename outFile

  (o,e) <- startProcess exec $ args <> [filename]

  if(null e) then
    return $ Right $ CompileResult 0 o outFile
  else
    return $ Left $ Error e

getHash :: ByteString -> ByteString
getHash = BC.takeWhile (/= '=') . BC.map toWebSafe . B64.encode . Crypto.hash
  where toWebSafe '/' = '_'
        toWebSafe '+' = '-'
        toWebSafe c   = c

