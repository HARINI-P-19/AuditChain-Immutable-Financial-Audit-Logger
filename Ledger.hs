-- | Ledger.hs
-- Pure functional operations on the immutable ledger.
-- append, query, filter, summarize — all without I/O.

module Ledger
  ( emptyLedger
  , appendEntry
  , ledgerBalance
  , ledgerSize
  , queryByActor
  , queryByType
  , queryByDateRange
  , totalDebits
  , totalCredits
  , summarize
  , LedgerSummary(..)
  ) where

import           Types
import           Hash          (computeHash, genesisHash)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Time     (UTCTime)
import           Data.List     (foldl', sortBy)
import           Data.Ord      (comparing)

-- ---------------------------------------------------------------------------
-- | An empty ledger — the starting state.
-- ---------------------------------------------------------------------------
emptyLedger :: Ledger
emptyLedger = []

-- ---------------------------------------------------------------------------
-- | Append a new transaction to the ledger.
--   Returns a NEW ledger — the original is never mutated.
--   This is the ONLY way to add entries; there is no update/delete.
-- ---------------------------------------------------------------------------
appendEntry :: Ledger -> Transaction -> UTCTime -> Ledger
appendEntry ledger tx ts =
  let idx   = length ledger
      prev  = if null ledger
                then genesisHash
                else entryHash (last ledger)
      h     = computeHash idx ts tx prev
      entry = AuditEntry
                { entryIndex = idx
                , entryTime  = ts
                , entryTx    = tx
                , prevHash   = prev
                , entryHash  = h
                }
  in  ledger ++ [entry]

-- ---------------------------------------------------------------------------
-- | Calculate running balance (Credits - Debits).
-- ---------------------------------------------------------------------------
ledgerBalance :: Ledger -> Double
ledgerBalance = foldl' step 0.0
  where
    step acc e =
      let amount = txAmount (entryTx e)
      in  case txType (entryTx e) of
            Credit -> acc + amount
            Debit  -> acc - amount

-- ---------------------------------------------------------------------------
-- | Number of entries in the ledger.
-- ---------------------------------------------------------------------------
ledgerSize :: Ledger -> Int
ledgerSize = length

-- ---------------------------------------------------------------------------
-- | Query entries by actor name (case-sensitive).
-- ---------------------------------------------------------------------------
queryByActor :: Text -> Ledger -> Ledger
queryByActor actor = filter (\e -> txActor (entryTx e) == actor)

-- ---------------------------------------------------------------------------
-- | Query entries by transaction type.
-- ---------------------------------------------------------------------------
queryByType :: TransactionType -> Ledger -> Ledger
queryByType tt = filter (\e -> txType (entryTx e) == tt)

-- ---------------------------------------------------------------------------
-- | Query entries within a UTC date range [from, to] inclusive.
-- ---------------------------------------------------------------------------
queryByDateRange :: UTCTime -> UTCTime -> Ledger -> Ledger
queryByDateRange from to =
  filter (\e -> entryTime e >= from && entryTime e <= to)

-- ---------------------------------------------------------------------------
-- | Sum of all debit amounts.
-- ---------------------------------------------------------------------------
totalDebits :: Ledger -> Double
totalDebits = sum . map (txAmount . entryTx) . filter isDebit
  where isDebit e = txType (entryTx e) == Debit

-- ---------------------------------------------------------------------------
-- | Sum of all credit amounts.
-- ---------------------------------------------------------------------------
totalCredits :: Ledger -> Double
totalCredits = sum . map (txAmount . entryTx) . filter isCredit
  where isCredit e = txType (entryTx e) == Credit

-- ---------------------------------------------------------------------------
-- | A summary record for the ledger.
-- ---------------------------------------------------------------------------
data LedgerSummary = LedgerSummary
  { sumEntries  :: !Int
  , sumCredits  :: !Double
  , sumDebits   :: !Double
  , sumBalance  :: !Double
  , sumActors   :: ![Text]
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- | Compute a full summary of the ledger using higher-order functions.
-- ---------------------------------------------------------------------------
summarize :: Ledger -> LedgerSummary
summarize ledger = LedgerSummary
  { sumEntries = ledgerSize   ledger
  , sumCredits = totalCredits ledger
  , sumDebits  = totalDebits  ledger
  , sumBalance = ledgerBalance ledger
  , sumActors  = uniqueActors  ledger
  }
  where
    uniqueActors = foldr addActor []
    addActor e acc =
      let a = txActor (entryTx e)
      in  if a `elem` acc then acc else a : acc
