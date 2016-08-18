module Process (startProcess)
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

writeToFile :: Handle -> FilePath -> IO ()
writeToFile handle path = 
    finally (withFile path WriteMode $ \hOut ->
                runEffect $ P.fromHandle handle >-> P.toHandle hOut)
            (hClose handle) 

startProcess :: String -> [String] -> IO (String, String)
startProcess name args = do
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
   (,) <$> (hGetContents hOut) <*> (hGetContents hErr)



