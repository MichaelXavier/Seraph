{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Seraph.Config ( load
                     , ConfigError(..) ) where

import Control.Applicative
import Control.Lens hiding (coerce)
import Control.Error
import Control.Exception ( SomeException
                         , try )
import Control.Monad
import Control.Monad.Trans
import qualified Data.Configurator as C
import Data.Configurator.Types ( Name
                               , Value(..)
                               , Worth(..))
import qualified Data.Map as M
import Data.Monoid
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text.Lens (unpacked)
import Data.Text.Strict.Lens (packed)

import Seraph.Types
import Seraph.Util

type RawConfig = HashMap Name Value

--FIXME: doesn't seem to catch parseerrors
load :: FilePath -> IO (Either ConfigError Config)
load fp = runEitherT $ do
  raw <- hoistEither . fmapL LoadException =<< lift (try . C.getMap =<< C.load [Required fp])
  progs <- hoistEither . concatMapM (uncurry parseProg) . unGroup $ raw
  return $ mempty & configured <>~ buildConfigured progs
  where
    buildConfigured progs = M.fromList [(p ^. name, p) | p <- progs]

--TODO: don't need a map
unGroup :: RawConfig -> [(ProgramId, RawConfig)]
unGroup = map convertKey . splitConfig firstDot
  where
    convertKey (nPrid, rc') = (nPrid ^. namePrid, rc')

parseProg :: ProgramId -> RawConfig -> Either ConfigError [Program]
parseProg prid rc = do
  count       <- lookupIntWithDefault "count" rc 1
  expandByCount prid rc count

--TODO: nat
expandByCount :: ProgramId -> RawConfig -> Int -> Either ConfigError [Program]
expandByCount basePrid rc count = forM prids $ \prid -> do
  exec'       <- lookupRequiredString      "exec"      rc
  delay'      <- lookupOptionalMaybeInt    "delay"     rc
  userName'   <- lookupOptionalMaybeString "user"      rc
  groupName'  <- lookupOptionalMaybeString "group"     rc
  stdout'     <- lookupOptionalMaybeString "stdout"    rc
  stderr'     <- lookupOptionalMaybeString "stderr"    rc
  workingDir' <- lookupOptionalMaybeString "directory" rc
  logExec'    <- lookupOptionalMaybeString "logger"    rc
  pidFile'    <- lookupOptionalMaybeString "pidfile"   rc
  termGrace'  <- lookupOptionalMaybeInt    "termgrace" rc
  env'        <- Right $ parseEnv          "env"       rc
  return $ Program prid
                   exec'
                   delay'
                   userName'
                   groupName'
                   stdout'
                   stderr'
                   workingDir'
                   logExec'
                   pidFile'
                   env'
                   termGrace'
  where
    prids = [ basePrid & pidStr <>~ ('-':show n) | n <- [1..count]]

firstDot :: Name -> Maybe (Name, Name)
firstDot n
  | T.null h    = Nothing
  | otherwise = Just (h ,rest)
  where
    (h, rest) = T.breakOn "." n & _2 %~ T.drop 1

splitConfig :: (Name -> Maybe (Name, Name)) -> RawConfig -> [(Name, RawConfig)]
splitConfig splitter = HM.toList . HM.foldrWithKey go mempty
  where
    go :: Name -> Value -> HashMap Name RawConfig -> HashMap Name RawConfig
    go k v acc = case splitter k of
                   Nothing -> acc
                   Just (master, rest) -> acc & ix master <>~ HM.singleton rest v

lookupRequiredString k      = coerce k "String" _String <=< lookupRequired k
lookupIntWithDefault k rc d = coerce k "Int" _Number $ fromMaybe (Number d) $ HM.lookup k rc
lookupOptionalMaybeInt k    = coerceMay k "Int" _Number <=< lookupOptional k
lookupOptionalMaybeString k = coerceMay k "String" _String <=< lookupOptional k
lookupRequired k            = note (MissingKey k) . HM.lookup k
lookupOptional k            = Right . HM.lookup k

coerce k t p v              = note (WrongType k t) $ v ^? p
coerceMay _ _ _ Nothing     = Right Nothing
coerceMay k t p (Just v)    = Just <$> coerce k t p v

parseEnv :: Name -> RawConfig -> [(String, String)]
parseEnv k = mapMaybe coercePair . HM.toList . mconcat . justConfigs . splitConfig splitKey
 where
   coercePair (_, String v) = Just (mempty, v ^. unpacked)
   coercePair _             = Nothing
   justConfigs = map snd
   splitKey n
     | T.null prefix = Just (prefix, postfix)
     | otherwise     = Nothing
     where
       (prefix, postfix) = T.breakOn (k <> ".") n

_String :: Simple Prism Value String
_String = prism (String . T.pack) coerceString
  where
    coerceString (String s) = Right $ T.unpack s
    coerceString x          = Left x

_Number :: Simple Prism Value Int
_Number = prism (Number . toRational) coerceNumber
  where
    coerceNumber (Number n) = Right $ truncate n
    coerceNumber x          = Left x

namePrid :: Iso' Name ProgramId
namePrid = iso (view (unpacked . to ProgramId)) (view (pidStr . packed))

data ConfigError = LoadException SomeException
                 | MissingKey Name
                 | WrongType Name String deriving (Show) -- probably a type error constructor too

