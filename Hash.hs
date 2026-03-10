-- | Hash.hs
-- Pure SHA-256 hashing and cryptographic chain verification.
-- No I/O here — all functions are referentially transparent.

module Hash
  ( computeHash
  , genesisHash
  , verifyEntry
  , verifyLedger
  , verifyLedgerVerbose
  ) where

import           Types
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base16 as B16
import           Data.Time              (UTCTime)

-- ---------------------------------------------------------------------------
-- | The genesis hash — used as prevHash for the very first entry.
--   A well-known constant, just like Bitcoin's genesis block.
-- ---------------------------------------------------------------------------
genesisHash :: Hash
genesisHash = T.replicate 64 (T.pack "0")

-- ---------------------------------------------------------------------------
-- | Compute the SHA-256 hash for a given entry's fields.
--   This is a PURE function: same inputs always produce same output.
--
--   Hash input = index || timestamp || txId || txType || amount || actor || desc || prevHash
-- ---------------------------------------------------------------------------
computeHash :: Int -> UTCTime -> Transaction -> Hash -> Hash
computeHash idx ts tx prev =
  let raw = T.concat
        [ T.pack (show idx)
        , T.pack (show ts)
        , txId tx
        , T.pack (show (txType tx))
        , T.pack (show (txAmount tx))
        , txActor tx
        , txDescription tx
        , prev
        ]
      bytes  = TE.encodeUtf8 raw
      hashed = SHA256.hash bytes
      hexed  = B16.encode hashed
  in  TE.decodeUtf8 hexed

-- ---------------------------------------------------------------------------
-- | Verify a single audit entry by recomputing its hash from scratch.
-- ---------------------------------------------------------------------------
verifyEntry :: AuditEntry -> Bool
verifyEntry e =
  let expected = computeHash
                   (entryIndex e)
                   (entryTime  e)
                   (entryTx    e)
                   (prevHash   e)
  in  entryHash e == expected

-- ---------------------------------------------------------------------------
-- | Verify the entire ledger.
--   Returns True only if ALL entries pass hash verification AND
--   every prevHash correctly points to the previous entry's hash.
-- ---------------------------------------------------------------------------
verifyLedger :: Ledger -> Bool
verifyLedger []  = True
verifyLedger [e] = verifyEntry e && prevHash e == genesisHash
verifyLedger ledger =
  all verifyEntry ledger
  && checkChain ledger
  where
    checkChain (e1:e2:rest) =
      entryHash e1 == prevHash e2 && checkChain (e2:rest)
    checkChain _ = True

-- ---------------------------------------------------------------------------
-- | Like verifyLedger but returns a list of (index, result, message).
-- ---------------------------------------------------------------------------
verifyLedgerVerbose :: Ledger -> [(Int, Bool, Text)]
verifyLedgerVerbose = map checkOne
  where
    checkOne e =
      let ok = verifyEntry e
          msg = if ok
                then T.pack "OK"
                else T.pack "HASH MISMATCH - entry may have been tampered with!"
      in  (entryIndex e, ok, msg)