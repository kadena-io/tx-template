{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Monad.Trans
import qualified Data.Aeson as A
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Data.YAML.Aeson as Y
import           Options.Applicative
------------------------------------------------------------------------------
import           TxTemplate
------------------------------------------------------------------------------

data GenArgs = GenArgs
  { _genArgs_templateFile :: FilePath
  , _genArgs_dataFile :: Maybe FilePath
  , _genArgs_outFilePat :: FilePath
  } deriving (Eq,Ord,Show)

data HolesArgs = HolesArgs
  { _holesArgs_templateFile :: FilePath
  } deriving (Eq,Ord,Show)

data Command
  = Gen GenArgs
  | Holes HolesArgs
  | Sign
  deriving (Eq,Ord,Show)

data Args = Args
  { _args_command :: Command
  } deriving (Eq,Ord,Show)

argP :: Parser Args
argP = Args
  <$> commands

genArgsP :: Parser GenArgs
genArgsP = GenArgs
  <$> strOption (long "template" <> short 't' <> metavar "YAML_FILE" <> help "Transaction template file")
  <*> optional (strOption (long "data" <> short 'd' <> metavar "YAML_FILE" <> help "Data file to get variables from"))
  <*> strOption (long "file-pat" <> short 'f' <> metavar "PATTERN" <> help "Pattern to use for output filenames (ex: \"tx-{{chain}}.yaml\")")

holesArgsP :: Parser HolesArgs
holesArgsP = HolesArgs
  <$> strOption (long "template" <> short 't' <> metavar "YAML_FILE" <> help "Transaction template file")

commands :: Parser Command
commands = hsubparser
  (  command "gen" (info (Gen <$> genArgsP)
       (progDesc "Generate unsigned transactions from a template"))
  <> command "holes" (info (Holes <$> holesArgsP)
       (progDesc "Get the holes for a template"))
  <> command "sign" (info (pure Sign)
       (progDesc "Sign unsigned transactions (not implemented yet)"))
  )

main :: IO ()
main = do
  args <- execParser opts
  case _args_command args of
    Gen ga -> genCmd ga
    Holes ha -> holesCmd ha
    Sign -> putStrLn "Not implemented yet"

opts :: ParserInfo Args
opts = info (argP <**> helper)
  (fullDesc <> header "tx-template - Kadena Tx Templater")

genCmd :: GenArgs -> IO ()
genCmd ga = do
  tplFile <- T.readFile $ _genArgs_templateFile ga
  res <- runExceptT $ do
    (tpl,vs) <- hoistEither $ parseAndGetVars tplFile
    dataText <- lift $ maybe (pure "{}") T.readFile $ _genArgs_dataFile ga
    vars <- hoistEither $ first show $ Y.decode1 (LB.fromStrict $ encodeUtf8 dataText)
    cmds <- hoistEither $ first prettyFailure $ fillValueVars tpl vars
    (fpTmpl, fpVars) <- hoistEither $ parseAndGetVars (T.pack $ _genArgs_outFilePat ga)
    fps <- hoistEither $ first prettyFailure $ fillFilenameVars fpTmpl (M.restrictKeys vars fpVars)
    let pairs = zip fps cmds
    lift $ mapM_ (\(fp,cmd) -> T.writeFile (T.unpack fp) cmd) pairs
    pure pairs
  case res of
    Left e -> error e
    Right pairs -> putStrLn $ "Wrote commands to: " <> show (map fst pairs)

holesCmd :: HolesArgs -> IO ()
holesCmd ha = do
  tplFile <- T.readFile $ _holesArgs_templateFile ha
  res <- runExceptT $ do
    (_,vs) <- hoistEither $ parseAndGetVars tplFile
    pure vs
  case res of
    Left e -> error e
    Right holes -> mapM_ (\h -> T.putStrLn $ h <> ": null") holes
