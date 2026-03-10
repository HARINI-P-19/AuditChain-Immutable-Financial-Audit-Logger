-- | Persist.hs
-- IO Monad-based persistence layer.
-- All file I/O is isolated here — the rest of the system stays pure.

module Persist
  ( saveLedger
  , loadLedger
  , ledgerFilePath
  , backupLedger
  ) where

import           Types
import           Data.Aeson           (encode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import           System.Directory     (doesFileExist, copyFile, createDirectoryIfMissing)
import           System.FilePath      ((</>), takeDirectory)
import           Data.Time            (getCurrentTime, formatTime, defaultTimeLocale)
import           Control.Exception    (try, SomeException)

-- ---------------------------------------------------------------------------
-- | Default ledger file path.
-- ---------------------------------------------------------------------------
ledgerFilePath :: FilePath
ledgerFilePath = "data/ledger.json"

-- ---------------------------------------------------------------------------
-- | Persist the ledger to disk as JSON.
--   Uses atomic write: write to temp file first, then rename.
-- ---------------------------------------------------------------------------
saveLedger :: FilePath -> Ledger -> IO (Either String ())
saveLedger path ledger = do
  createDirectoryIfMissing True (takeDirectory path)
  result <- try (BL.writeFile path (encode ledger)) :: IO (Either SomeException ())
  case result of
    Left err -> return $ Left ("Failed to save ledger: " ++ show err)
    Right _  -> return $ Right ()

-- ---------------------------------------------------------------------------
-- | Load the ledger from disk.
--   Returns an empty ledger if the file does not exist yet.
-- ---------------------------------------------------------------------------
loadLedger :: FilePath -> IO (Either String Ledger)
loadLedger path = do
  exists <- doesFileExist path
  if not exists
    then return (Right [])     -- first run: start with empty ledger
    else do
      result <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
      case result of
        Left err  -> return $ Left ("Failed to read ledger: " ++ show err)
        Right raw ->
          case eitherDecode raw of
            Left parseErr -> return $ Left ("Corrupt ledger JSON: " ++ parseErr)
            Right ledger  -> return $ Right ledger

-- ---------------------------------------------------------------------------
-- | Create a timestamped backup of the ledger file.
-- ---------------------------------------------------------------------------
backupLedger :: FilePath -> IO (Either String FilePath)
backupLedger path = do
  exists <- doesFileExist path
  if not exists
    then return (Left "No ledger file to backup.")
    else do
      now <- getCurrentTime
      let ts         = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
          backupPath = takeDirectory path </> "backups" </> "ledger_" ++ ts ++ ".json"
      createDirectoryIfMissing True (takeDirectory backupPath)
      result <- try (copyFile path backupPath) :: IO (Either SomeException ())
      case result of
        Left err -> return $ Left ("Backup failed: " ++ show err)
        Right _  -> return $ Right backupPath
