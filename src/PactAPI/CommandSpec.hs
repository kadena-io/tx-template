{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PactAPI.CommandSpec where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Char
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Time.Clock.POSIX
import           GHC.Generics
import qualified Pact.Types.Capability as Pact
import qualified Pact.Types.ChainId as Pact
import qualified Pact.Types.ChainMeta as Pact
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Gas as Pact
import qualified Pact.Types.RPC as Pact
import qualified Pact.Types.Scheme as Pact
import qualified Pact.Types.SPV as Pact
import qualified Pact.Types.Term as Pact
------------------------------------------------------------------------------

resultToEither :: Result a -> Either String a
resultToEither (Success s) = Right s
resultToEither (Error s) = Left s

parseB16JSON :: Value -> Parser ByteString
parseB16JSON = withText "Base16" parseB16Text
{-# INLINE parseB16JSON #-}

parseB16Text :: Text -> Parser ByteString
parseB16Text t = case B16.decode (encodeUtf8 t) of
                 (s,leftovers) | leftovers == B.empty -> return s
                               | otherwise -> fail $ "Base16 decode failed: " ++ show t
{-# INLINE parseB16Text #-}

parseB16TextOnly :: Text -> Either String ByteString
parseB16TextOnly t = resultToEither $ parse parseB16Text t

toB16JSON :: ByteString -> Value
toB16JSON s = String $ toB16Text s

toB16Text :: ByteString -> Text
toB16Text s = decodeUtf8 $ B16.encode s

newtype Base16BS = Base16BS { unBase16BS :: ByteString }
  deriving (Eq, Generic)
instance ToJSON Base16BS where
  toJSON (Base16BS p) = toB16JSON p
instance FromJSON Base16BS where
  parseJSON = withText "Base16BS" $ \s -> do
    s' <- parseB16Text s
    return $ Base16BS s'
instance Show Base16BS where
  show (Base16BS b) = T.unpack $ toB16Text b
instance ToJSONKey Base16BS where
    toJSONKey = toJSONKeyText (toB16Text . unBase16BS)
    {-# INLINE toJSONKey #-}
instance FromJSONKey Base16BS where
    fromJSONKey = FromJSONKeyTextParser (either fail (return . Base16BS) . parseB16TextOnly)
    {-# INLINE fromJSONKey #-}

lensyToJSON
  :: (Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
lensyToJSON n = genericToJSON (lensyOptions n)

lensyParseJSON
  :: (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
lensyParseJSON n = genericParseJSON (lensyOptions n)

lensyOptions :: Int -> Options
lensyOptions n = defaultOptions { fieldLabelModifier = lensyConstructorToNiceJson n }

lensyConstructorToNiceJson :: Int -> String -> String
lensyConstructorToNiceJson n fieldName = firstToLower $ drop n fieldName
  where
    firstToLower (c:cs) = toLower c : cs
    firstToLower _ = error $ "lensyConstructorToNiceJson: bad arguments: " ++ show (n,fieldName)

--data ApiReq = ApiReq {
--  _ylType :: Maybe Text,
--  _ylPactTxHash :: Maybe Hash,
--  _ylStep :: Maybe Int,
--  _ylRollback :: Maybe Bool,
--  _ylData :: Maybe Value,
--  _ylProof :: Maybe ContProof,
--  _ylDataFile :: Maybe FilePath,
--  _ylCode :: Maybe Text,
--  _ylCodeFile :: Maybe FilePath,
--  _ylKeyPairs :: Maybe [ApiKeyPair],
--  _ylSigners :: Maybe [ApiSigner],
--  _ylNonce :: Maybe Text,
--  _ylPublicMeta :: Maybe ApiPublicMeta,
--  _ylNetworkId :: Maybe NetworkId
--  } deriving (Eq,Show,Generic)

--data CommandType = ExecCommand | ContCommand
--  deriving (Eq,Enum,Bounded,Generic)
--
--instance Show CommandType where
--  show ExecCommand = "exec"
--  show ContCommand = "cont"

--type CommandSpec = Pact.Payload Pact.PublicMeta Pact.ParsedCode

data CommandSpec = CommandSpec
  { _commandSpec_networkId :: Maybe Text
  --, _commandSpec_payload :: Pact.PactRPC Pact.ParsedCode
  , _commandSpec_payload :: CommandPayload
  , _commandSpec_publicMeta :: PublicMeta
  , _commandSpec_signers :: Maybe [Signer]
  , _commandSpec_nonce :: Maybe Text
  } deriving (Eq,Show,Generic)

-- TODO Might want to auto-add a fixed creation time

instance ToJSON CommandSpec where
  toJSON (CommandSpec net p pm s nonce) = object $
    (payloadPairs p <>
    [ "networkId" .= net
    , "publicMeta" .= pm
    , "signers" .= s
    , "nonce" .= nonce
    ])

instance FromJSON CommandSpec where
  parseJSON = withObject "CommandSpec" $ \o -> do
    mt <- o .:? "type"
    let espec = fmap ExecCommand $ ExecSpec
                  <$> o .: "code"
                  <*> o .: "data"
    p <- case mt of
           Just (String "exec") -> espec
           Just (String "cont") -> fmap ContCommand $ ContSpec
                                     <$> o .: "pactTxHash"
                                     <*> o .: "step"
                                     <*> o .: "rollback"
                                     <*> o .: "data"
                                     <*> o .: "proof"
           Nothing -> espec
           Just (String s) -> fail $ "CommandSpec 'type' field has invalid value " <> (T.unpack s)
           Just t -> typeMismatch "String" t
    CommandSpec
      <$> o .:? "networkId"
      <*> pure p
      <*> o .: "publicMeta"
      <*> o .: "signers"
      <*> o .:? "nonce"

data CommandPayload
  = ExecCommand ExecSpec
  | ContCommand ContSpec
  deriving (Eq,Show,Generic)

--payloadToPactRPC :: CommandPayload -> PactRPC Text
--payloadToPactRPC = \case
--  ExecCommand (ExecSpec c d) -> Pact.Exec (Pact.ExecMsg c d)
--  ContCommand (ContSpec h s r d p) -> Pact.Continuation (Pact.ContMsg h s r d p)

payloadPairs :: CommandPayload -> [Pair]
payloadPairs (ExecCommand e) =
  ( "type" .= String "exec") : execSpecPairs e
payloadPairs (ContCommand c) =
  ( "type" .= String "cont") : contSpecPairs c

data ExecSpec = ExecSpec
  { _execSpec_code :: Text
  , _execSpec_data :: Value
  } deriving (Eq,Show,Generic)

execSpecPairs :: ExecSpec -> [Pair]
execSpecPairs (ExecSpec c d) =
  [ "code" .= c
  , "data" .= d
  ]

data ContSpec = ContSpec
  { _contSpec_pactTxHash :: Text
  , _contSpec_step :: Int
  , _contSpec_rollback :: Bool
  , _contSpec_data :: Value
  , _contSpec_proof :: Maybe ContProof
  } deriving (Eq,Show,Generic)

contSpecPairs :: ContSpec -> [Pair]
contSpecPairs (ContSpec h s r d p) =
  [ "pactTxHash" .= h
  , "step" .= s
  , "rollback" .= r
  , "data" .= d
  , "proof" .= p
  ]

newtype ContProof = ContProof { unContProof :: Text }
  deriving (Eq,Show,Generic,ToJSON,FromJSON)

data Signer = Signer
 { _signer_scheme :: (Maybe Pact.PPKScheme)
 , _signer_pubKey :: Text
 , _signer_address :: (Maybe Text)
 , _signer_capList :: [Pact.SigCapability]
 } deriving (Eq, Show, Generic)

instance ToJSON Signer where
  toJSON Signer{..} = object $
    consMay "scheme" _signer_scheme $
    consMay "addr" _signer_address $
    -- TODO Should this be "caps" or "clist"?
    consListMay "caps" _signer_capList $
    -- TODO Should this be "public" or "pubkey"?

    -- "caps" and "public" are used by Pact's ApiSigner type in ApiReq.hs but clist and pubKey are used by Signer.
    [ "public" .= _signer_pubKey ]
    where
      consMay f mv ol = maybe ol (consPair f ol) mv
      consPair f ol v = (f .= v):ol
      consListMay f cl ol
        | null cl = ol
        | otherwise = consPair f ol cl

instance FromJSON Signer where
  parseJSON = withObject "Signer" $ \o -> Signer
    <$> o .:? "scheme"
    <*> o .: "public"
    <*> o .:? "addr"
    <*> (listMay <$> (o .:? "caps"))
    where
      listMay = fromMaybe []

toPactSigner :: Signer -> Pact.Signer
toPactSigner Signer{..} = Pact.Signer _signer_scheme _signer_pubKey _signer_address _signer_capList

--data SigCapability = SigCapability
--  { _scName :: Text
--  , _scArgs :: ![PactValue]
--  } deriving (Eq,Show,Generic)
--
--instance ToJSON SigCapability where
--  toJSON (SigCapability n args) = object $
--    [ "name" .=  n
--    , "args" .= args
--    ]
--
--instance FromJSON SigCapability where
--  parseJSON = withObject "SigCapability" $ \o -> SigCapability
--    <$> o .: "name"
--    <*> o .: "args"

type PactValue = Value

-- This is different from Pact's PublicMata type in that the creationtime field
-- is a Maybe. Since we use tihs for JSON/YAML decoding this allows us let the
-- user skip the creationtime field and we'll automatically fill it in with the
-- current time and we can do it without needing IO to decode.
data PublicMeta = PublicMeta
  { _pmChainId :: Pact.ChainId
    -- ^ platform-specific chain identifier, e.g. "0"
  , _pmSender :: Text
    -- ^ sender gas account key
  , _pmGasLimit :: Pact.GasLimit
    -- ^ gas limit (maximum acceptable gas units for tx)
  , _pmGasPrice :: Pact.GasPrice
    -- ^ per-unit gas price
  , _pmTTL :: Pact.TTLSeconds
    -- ^ TTL in seconds
  , _pmCreationTime :: Maybe Pact.TxCreationTime
    -- ^ Creation time in seconds since UNIX epoch
  } deriving (Eq, Show, Generic)

instance ToJSON PublicMeta where
  toJSON (PublicMeta cid s gl gp ttl ct) = object
    [ "chainId" .= cid
    , "sender" .= s
    , "gasLimit" .= gl
    , "gasPrice" .= gp
    , "ttl" .= ttl
    , "creationTime" .= ct
    ]

instance FromJSON PublicMeta where
  parseJSON = withObject "PublicMeta" $ \o -> PublicMeta
    <$> o .: "chainId"
    <*> o .: "sender"
    <*> o .: "gasLimit"
    <*> o .: "gasPrice"
    <*> o .: "ttl"
    <*> o .:? "creationTime"

mkPactMeta :: POSIXTime -> PublicMeta -> Pact.PublicMeta
mkPactMeta creationTime PublicMeta{..} =
  Pact.PublicMeta
    _pmChainId
    _pmSender
    _pmGasLimit
    _pmGasPrice
    _pmTTL
    (fromMaybe (Pact.TxCreationTime $ round creationTime) _pmCreationTime)

-- These not needed at the moment...switched to Pact types
--type ChainId = Text
--type GasLimit = Integer
--type GasPrice = ParsedDecimal
--type TTLSeconds = Integer
--type TxCreationTime = Integer

-- | This function is roughly analogous to mkApiReqCmd in
-- pact/src-ghc/Pact/ApiReq.hs. The main difference is that this function and
-- the associated types in this module don't support the @codeFile@ and
-- @dataFile@ fields or the @keyPairs@ field which aren't appropriate in the
-- context of a web browser.
commandSpecToPayload :: POSIXTime -> CommandSpec -> Pact.Payload Pact.PublicMeta Text
commandSpecToPayload creationTime CommandSpec{..} = Pact.Payload
    rpc
    ("sigtool " <> T.pack (show creationTime))
    (mkPactMeta creationTime _commandSpec_publicMeta)
    (maybe [] (map toPactSigner) _commandSpec_signers)
    (Just $ maybe (Pact.NetworkId "mainnet01") Pact.NetworkId _commandSpec_networkId)
  where
    rpc = case _commandSpec_payload of
      ExecCommand (ExecSpec c d) -> Pact.Exec (Pact.ExecMsg c d)
      ContCommand (ContSpec h s r d p) -> Pact.Continuation $
        Pact.ContMsg (Pact.PactId h) s r d (Pact.ContProof . encodeUtf8 . unContProof <$> p)
