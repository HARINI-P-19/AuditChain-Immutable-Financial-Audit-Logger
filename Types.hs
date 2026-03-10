{-# LANGUAGE DeriveGeneric #-}

-- | Types.hs
-- Core algebraic data types for AuditChain.
-- All types are immutable by construction (Haskell default).

module Types
  ( TransactionType(..)
  , Transaction(..)
  , AuditEntry(..)
  , Ledger
  , Hash
  , mkTransaction
  ) where

import           Data.Text    (Text)
import qualified Data.Text    as T
import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)
import           Data.Aeson   (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- | A transaction can only be a Debit or a Credit.
--   Using a sum type makes any other value unrepresentable at compile time.
-- ---------------------------------------------------------------------------
data TransactionType = Debit | Credit
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance ToJSON   TransactionType
instance FromJSON TransactionType

-- ---------------------------------------------------------------------------
-- | A financial transaction record.
-- ---------------------------------------------------------------------------
data Transaction = Transaction
  { txId          :: !Text           -- ^ Unique identifier (UUID)
  , txType        :: !TransactionType
  , txAmount      :: !Double          -- ^ Amount in base currency units
  , txActor       :: !Text           -- ^ Entity initiating the transaction
  , txDescription :: !Text           -- ^ Human-readable description
  } deriving (Show, Eq, Generic)

instance ToJSON   Transaction
instance FromJSON Transaction

-- ---------------------------------------------------------------------------
-- | A single entry in the immutable audit ledger.
--   Each entry stores its own hash AND the previous entry's hash,
--   forming a cryptographic chain.
-- ---------------------------------------------------------------------------
data AuditEntry = AuditEntry
  { entryIndex   :: !Int             -- ^ Sequential index (0-based)
  , entryTime    :: !UTCTime         -- ^ UTC timestamp of recording
  , entryTx      :: !Transaction     -- ^ The financial transaction
  , prevHash     :: !Hash            -- ^ Hash of the previous entry
  , entryHash    :: !Hash            -- ^ SHA-256 hash of this entry's contents
  } deriving (Show, Eq, Generic)

instance ToJSON   AuditEntry
instance FromJSON AuditEntry

-- ---------------------------------------------------------------------------
-- | The ledger is a simple immutable list of audit entries.
--   New entries are only ever appended; existing entries are never modified.
-- ---------------------------------------------------------------------------
type Ledger = [AuditEntry]

-- ---------------------------------------------------------------------------
-- | A SHA-256 hex-encoded hash string.
-- ---------------------------------------------------------------------------
type Hash = Text

-- ---------------------------------------------------------------------------
-- | Smart constructor for Transaction.
-- ---------------------------------------------------------------------------
mkTransaction :: Text -> TransactionType -> Double -> Text -> Text -> Transaction
mkTransaction = Transaction
