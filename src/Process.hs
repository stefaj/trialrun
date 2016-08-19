module Process (startProcess, 
                Time(..), 
                ProcessResult(..)
               )
  where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Pipes
import qualified Pipes.ByteString as P
import Pipes.Concurrent
import System.Process
import System.IO
import System.Exit
import Criterion.Measurement
import System.Timeout
import Error

writeToFile :: Handle -> FilePath -> IO ()
writeToFile handle path = 
    finally (withFile path WriteMode $ \hOut ->
                runEffect $ P.fromHandle handle >-> P.toHandle hOut)
            (hClose handle) 

data ProcessResult = ProcessResult {pr_output :: String, pr_time :: Double}

data Time = Microseconds Int

startProcess :: String -> [String] -> Time -> IO (Either Error ProcessResult)
startProcess name args (Microseconds maxTime) = do
   (_,mOut,mErr,procHandle) <- createProcess $ 
        (proc name args) { std_out = CreatePipe
                                , std_err = CreatePipe 
                                }
   let (hOut,hErr) = maybe (error "bogus handles") 
                           id
                           ((,) <$> mOut <*> mErr)
--    a1 <- async $ writeToFile hOut "stdout.txt" 
--    a2 <- async $ writeToFile hErr "stderr.txt" 
--    waitBoth a1 a2o
--
   t <- getTime
   exitCode <- timeout maxTime $ waitForProcess procHandle
   case exitCode of 
    Nothing -> do
            terminateProcess procHandle
            return $ Left $ Timeout
    Just ExitSuccess -> do res <- hGetContents hOut
                           t' <- getTime
                           return $ Right $ ProcessResult res (t' - t)
    Just (ExitFailure _) -> do res <- hGetContents hErr
                               return $ Left $ Timeout 
    
