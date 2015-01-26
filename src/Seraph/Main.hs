module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Control.Error
import           Control.Monad       (void)
import           Control.Monad.Trans (lift)
import           Data.Monoid         (mempty)
import           MVC                 (runMVC)
import           System.Environment  (getArgs)
import           System.Exit
import           System.IO           (hPutStrLn, stderr)
-------------------------------------------------------------------------------
import           Seraph.Config       (ConfigError, load)
import           Seraph.Core         (core)
import           Seraph.Model        (oracleModel)
-------------------------------------------------------------------------------

main :: IO ()
main = do
  initAndFp <- runEitherT $ do
    fp <- tryHead NoConfig =<< lift getArgs
    initCfg <- load' fp
    return (initCfg, fp)
  either bail (uncurry boot) initAndFp
  where
    boot initCfg fp = void $ runMVC mempty oracleModel (core initCfg fp)
    bail NoConfig = errorExit "Please pass a config"
    bail (InvalidConfig e) = errorExit ("Invalid config " ++ show e)
    load' fp = hoistEither . fmapL InvalidConfig =<< lift (load fp)


-------------------------------------------------------------------------------
data BootError = NoConfig | InvalidConfig ConfigError


-------------------------------------------------------------------------------
errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg' >> exitFailure
  where
    msg' = "ERROR: " ++ msg
