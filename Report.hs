-- | Report.hs
-- Audit report generation

module Report
  ( printLedger
  , printEntry
  , printSummary
  , printVerification
  , printHeader
  , printSeparator
  , formatAmount
  , formatHash
  ) where

import           Types
import           Ledger     (summarize, LedgerSummary(..))
import           Hash       (verifyLedger, verifyLedgerVerbose)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Data.Time  (formatTime, defaultTimeLocale)
import           Data.List  (intercalate)

green, red, yellow, cyan, bold, reset :: String
green  = "\ESC[32m"
red    = "\ESC[31m"
yellow = "\ESC[33m"
cyan   = "\ESC[36m"
bold   = "\ESC[1m"
reset  = "\ESC[0m"

printHeader :: IO ()
printHeader = do
  putStrLn $ bold ++ cyan
  putStrLn "=========================================================="
  putStrLn "            AuditChain - Financial Audit System"
  putStrLn "        23CSE212 Principles of Functional Language"
  putStrLn "=========================================================="
  putStrLn reset

printSeparator :: IO ()
printSeparator = putStrLn $ replicate 60 '-'

formatAmount :: TransactionType -> Double -> String
formatAmount Credit amt = green ++ "+ $" ++ show amt ++ reset
formatAmount Debit  amt = red   ++ "- $" ++ show amt ++ reset

formatHash :: Hash -> String
formatHash h = T.unpack (T.take 16 h) ++ "..."

printEntry :: AuditEntry -> IO ()
printEntry e = do
  let tx = entryTx e
      ts = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (entryTime e)
  putStrLn $ "[" ++ show (entryIndex e) ++ "] " ++ ts
  putStrLn $ "  Actor : " ++ T.unpack (txActor tx)
  putStrLn $ "  Desc  : " ++ T.unpack (txDescription tx)
  putStrLn $ "  TxID  : " ++ T.unpack (txId tx)
  putStrLn $ "  Hash  : " ++ formatHash (entryHash e)
  putStrLn ""

printLedger :: Ledger -> IO ()
printLedger [] = do
  putStrLn "No entries in ledger."
  putStrLn ""
printLedger ledger = do
  putStrLn "\nAudit Ledger\n"
  mapM_ printEntry ledger

printSummary :: Ledger -> IO ()
printSummary ledger = do
  let s = summarize ledger
  putStrLn "\nLedger Summary\n"
  putStrLn $ "Total Entries : " ++ show (sumEntries s)
  putStrLn $ "Total Credits : $" ++ show (sumCredits s)
  putStrLn $ "Total Debits  : $" ++ show (sumDebits s)
  putStrLn $ "Net Balance   : $" ++ show (sumBalance s)
  putStrLn $ "Actors        : " ++ intercalate ", " (map T.unpack (sumActors s))
  putStrLn ""

printVerification :: Ledger -> IO ()
printVerification [] = do
  putStrLn "Ledger empty. Nothing to verify."
  putStrLn ""
printVerification ledger = do
  putStrLn "\nIntegrity Verification\n"
  let results = verifyLedgerVerbose ledger
  mapM_ printResult results
  if verifyLedger ledger
    then putStrLn "\nLedger integrity VERIFIED.\n"
    else putStrLn "\nIntegrity FAILURE detected.\n"

printResult :: (Int, Bool, Text) -> IO ()
printResult (idx, ok, msg) =
  if ok
     then putStrLn $ "[OK] Entry " ++ show idx ++ " - " ++ T.unpack msg
     else putStrLn $ "[FAIL] Entry " ++ show idx ++ " - " ++ T.unpack msg