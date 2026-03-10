-- | Main.hs
-- AuditChain - Immutable Financial Audit Logger
-- CLI entry point.

module Main where

import           Types
import           Hash     (verifyLedger)
import           Ledger   (appendEntry, emptyLedger, queryByActor, queryByType,
                            ledgerBalance, ledgerSize)
import           Persist  (saveLedger, loadLedger, backupLedger, ledgerFilePath)
import           Report   (printLedger, printSummary, printVerification,
                            printHeader, printSeparator)

import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Time      (getCurrentTime)
import           Data.IORef     (newIORef, readIORef, writeIORef, IORef)
import           System.IO      (hFlush, stdout, hSetBuffering, BufferMode(..))
import           System.Exit    (exitSuccess)
import           Data.UUID.V4   (nextRandom)
import           Data.UUID      (toText)
import           Control.Monad  (when)
import           Text.Read      (readMaybe)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  printHeader

  putStrLn "  Loading ledger from disk..."
  result <- loadLedger ledgerFilePath
  case result of
    Left err -> do
      putStrLn $ "  ERROR: " ++ err
      putStrLn "  Starting with empty ledger."
      ref <- newIORef emptyLedger
      mainLoop ref

    Right ledger -> do
      let n = ledgerSize ledger
      if n == 0
        then putStrLn "  No existing ledger found. Starting fresh.\n"
        else do
          putStr $ "  Loaded " ++ show n ++ " entries. "
          if verifyLedger ledger
            then putStrLn "Integrity: VERIFIED\n"
            else putStrLn "Integrity: FAILED - possible tampering!\n"

      ref <- newIORef ledger
      mainLoop ref


mainLoop :: IORef Ledger -> IO ()
mainLoop ref = do
  printMenu
  putStr "  Enter choice: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> addTransaction ref >> mainLoop ref
    "2" -> viewLedger ref >> mainLoop ref
    "3" -> viewSummary ref >> mainLoop ref
    "4" -> runVerify ref >> mainLoop ref
    "5" -> queryMenu ref >> mainLoop ref
    "6" -> doBackup >> mainLoop ref
    "7" -> do
             putStrLn "\n  Goodbye. Your ledger has been saved.\n"
             exitSuccess
    _   -> do
             putStrLn "\n  Invalid option. Please try again."
             mainLoop ref


printMenu :: IO ()
printMenu = do
  printSeparator
  putStrLn "  MENU"
  printSeparator
  putStrLn "  1. Add Transaction"
  putStrLn "  2. View Full Ledger"
  putStrLn "  3. Ledger Summary"
  putStrLn "  4. Verify Integrity"
  putStrLn "  5. Query Ledger"
  putStrLn "  6. Backup Ledger"
  putStrLn "  7. Exit"
  printSeparator


addTransaction :: IORef Ledger -> IO ()
addTransaction ref = do
  putStrLn "\n  -- Add New Transaction --\n"

  putStr "  Type (1=Credit, 2=Debit): "
  hFlush stdout
  typeInput <- getLine

  let txTypeVal = case typeInput of
                    "1" -> Just Credit
                    "2" -> Just Debit
                    _   -> Nothing

  case txTypeVal of
    Nothing -> putStrLn "  Invalid type. Aborting.\n"

    Just tt -> do
      putStr "  Amount (e.g. 1500.00): "
      hFlush stdout
      amtInput <- getLine

      case readMaybe amtInput :: Maybe Double of
        Nothing -> putStrLn "  Invalid amount. Aborting.\n"

        Just amt -> do
          when (amt <= 0) $
            putStrLn "  Warning: amount should be positive."

          putStr "  Actor (e.g. TreasuryCorp): "
          hFlush stdout
          actor <- getLine

          putStr "  Description: "
          hFlush stdout
          desc <- getLine

          uuid <- nextRandom
          let txid = toText uuid
              tx   = mkTransaction txid tt amt (T.pack actor) (T.pack desc)

          ts <- getCurrentTime
          ledger <- readIORef ref
          let newLedger = appendEntry ledger tx ts

          saveResult <- saveLedger ledgerFilePath newLedger
          case saveResult of
            Left err ->
              putStrLn $ "\n  ERROR saving: " ++ err

            Right _ -> do
              writeIORef ref newLedger
              putStrLn "\n  Transaction recorded successfully!"
              putStrLn $ "  Entry #" ++ show (ledgerSize newLedger - 1)
                       ++ " | Balance: $" ++ show (ledgerBalance newLedger)
              putStrLn ""


viewLedger :: IORef Ledger -> IO ()
viewLedger ref = do
  ledger <- readIORef ref
  printLedger ledger


viewSummary :: IORef Ledger -> IO ()
viewSummary ref = do
  ledger <- readIORef ref
  printSummary ledger


runVerify :: IORef Ledger -> IO ()
runVerify ref = do
  ledger <- readIORef ref
  printVerification ledger


queryMenu :: IORef Ledger -> IO ()
queryMenu ref = do
  putStrLn "\n  -- Query Ledger --\n"
  putStrLn "  1. Filter by Actor"
  putStrLn "  2. Show only Credits"
  putStrLn "  3. Show only Debits"

  putStr "\n  Enter choice: "
  hFlush stdout
  c <- getLine

  ledger <- readIORef ref

  case c of
    "1" -> do
      putStr "  Actor name: "
      hFlush stdout
      actor <- getLine

      let results = queryByActor (T.pack actor) ledger
      putStrLn $ "\n  Found " ++ show (length results) ++ " entries."
      printLedger results

    "2" -> do
      let results = queryByType Credit ledger
      putStrLn $ "\n  Credits (" ++ show (length results) ++ " entries):"
      printLedger results

    "3" -> do
      let results = queryByType Debit ledger
      putStrLn $ "\n  Debits (" ++ show (length results) ++ " entries):"
      printLedger results

    _ -> putStrLn "  Invalid option.\n"


doBackup :: IO ()
doBackup = do
  result <- backupLedger ledgerFilePath
  case result of
    Left err ->
      putStrLn $ "\n  ERROR: " ++ err

    Right path ->
      putStrLn $ "\n  Backup saved to: " ++ path ++ "\n"