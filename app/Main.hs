module Main where

import AppMain (appMain)
import Database (initDB)
import System.Console.ANSI (clearScreen)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  -- Clear the terminal screen before we do anything
  clearScreen
  putStrLn "Initalising DB.."
  -- To init and create DB file if not exists,
  -- return the connection
  conn <- initDB
  putStrLn "Connection established.\n"
  -- Turn off output buffering
  hSetBuffering stdout NoBuffering
  -- Start the main menu
  appMain conn
