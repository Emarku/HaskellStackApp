module AppMain
  ( -- * AppMain

    --

    -- | This module contains all functions for thr main menu
    appMain,
  )
where

import AppConfig (baseURL, imdbSearchCat, imdbSearchUrl, imdbTopUrl)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isJust)
import Database
import Database.SQLite.Simple (Connection, close)
import Fetch
import MiscUtils (charReplace, listStringToInt, putStrLnListWithNumber, seperator)
import Parse
import System.Console.ANSI (clearScreen)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import Types

menu :: [String]
menu =
  [ "Download top 100 movie data",
    "Download movie data by title",
    "Search movies",
    "Show all movies by title",
    "Export database as JSON",
    "Delete movies",
    "Reset database",
    "Quit"
  ]

dbSearchOptions :: [String]
dbSearchOptions =
  [ "By title",
    "By genre",
    "By country of origin",
    "By actor name",
    "Return to main menu"
  ]

-- | Clear screen after user input
cls :: IO ()
cls = do
  putStrLn "Press ENTER to continue.."
  getLine
  clearScreen

-- | display the main menu
showMenu :: IO ()
showMenu = do
  putStrLn "    Welcome to the Haskell IMDb app"
  putStrLn $ seperator '-' 39
  putStrLnListWithNumber menu
  putStrLn $ seperator '-' 39
  putStr "Choose an option > "

-- | Batch page crawl and insertion
downloadAndInsertPages :: Connection -> [String] -> IO ()
downloadAndInsertPages conn urls = do
  putStrLn "Downloading all selected movies content"
  pages <- downloadList urls
  putStrLn "Parsing all movie pages"
  movs <- parseHTMLPages pages
  putStrLn "Writing to DB..."
  beginInsertMovieTransactions conn movs
  putStrLn "Done"

-- | Get the top 250 page
-- We only get 100 now as IMDB may temporary block your IP upon
-- a massive page crawl
getTopHundred :: Connection -> IO ()
getTopHundred conn = do
  putStrLn "Getting top 100 page"
  htmlRaw <- download imdbTopUrl
  putStrLn "Processing all movie URLs"
  downloadAndInsertPages conn (take 100 $ getAllMovieURL htmlRaw)
  cls

-- | Find a movie on IMDB site using the advanced search feature of IMDB
searchMovieOnline :: Connection -> IO ()
searchMovieOnline conn = do
  putStr "Please enter a keyword > "
  keyword <- getLine
  let searchURL = imdbSearchUrl ++ charReplace ' ' '+' keyword ++ imdbSearchCat
  pageHTML <- download searchURL
  let result = getSearchResult pageHTML
  handleSearchResult conn result
  clearScreen

-- | Display the search result to user and accept user input to download movies
handleSearchResult :: Connection -> [(String, String)] -> IO ()
handleSearchResult conn result = do
  let titles = map fst result
  let urls = map snd result
  putStrLn "Online search result:"
  if null titles
    then do
      putStrLn "No movie has been found."
      cls
    else do
      putStrLnListWithNumber titles
      putStrLn "\nType 0 to return"
      putStrLn "To download, type a number to select a movie or select multiple movie details using ',' to sepeate each number."
      putStrLn "Example: 76 or 3,10,22"
      putStr "Your choice > "
      userIn <- getLine
      let idxStrList = splitOn "," userIn
      let idxMaybes = listStringToInt idxStrList
      if all isJust idxMaybes && length idxMaybes <= length result
        then do
          let idxs = catMaybes idxMaybes
          if length idxs == 1 && head idxs == 0
            then clearScreen
            else do
              if all (<= length result) idxs && all (> 0) idxs
                then do
                  let urlsList = map ((urls !!) . (\i -> i - 1)) idxs
                  downloadAndInsertPages conn urlsList
                  cls
                  handleSearchResult conn result
                else do
                  putStrLn "Invalid input, please try again."
                  cls
                  handleSearchResult conn result
        else do
          putStrLn "Invalid input, please try again."
          cls
          handleSearchResult conn result

-- | Ask for the filename and dump everything in the db to json
dumpDBToJSON :: Connection -> IO ()
dumpDBToJSON conn = do
  putStr "Enter the filename > "
  filename <- getLine
  putStrLn "Getting all entries from DB"
  movs <- queryAllMovies conn
  putStrLn "Exporting as JSON"
  dumpAllToJSONFile filename movs
  putStrLn "Done"
  cls

-- | Query all movies and pass it to showMovieTitles function
showAllMovieTitles :: Connection -> IO ()
showAllMovieTitles conn = do
  clearScreen
  movs <- queryAllMovies conn
  showMovieTitles movs
  clearScreen

-- | Display all movie titles of a give Movie list
-- User can input a index number to view the details
showMovieTitles :: [Movie] -> IO ()
showMovieTitles movs = do
  clearScreen
  putStrLn "Database search result:"
  let titleLst = map title movs
  if null titleLst
    then do
      putStrLn "No movie has been found."
      cls
    else do
      putStrLnListWithNumber titleLst
      putStrLn "\nType a number to view the details of the movie or type 0 to return"
      putStr "Your choice > "
      userIn <- getLine
      let idx = readMaybe userIn :: Maybe Int
      case idx of
        Just n -> do
          case n of
            0 -> clearScreen
            _ -> do
              if n > length movs || n < 0
                then do
                  putStrLn "Invalid number, please try again."
                  cls
                  showMovieTitles movs
                else do
                  movieToString $ movs !! (n - 1)
                  cls
                  showMovieTitles movs
        Nothing -> do
          putStrLn "Invalid input, please try again."
          cls
          showMovieTitles movs

--- | 4 types of queries to the database
searchMovInDB :: Connection -> IO ()
searchMovInDB conn = do
  clearScreen
  putStrLn "Search options"
  putStrLnListWithNumber dbSearchOptions
  putStr "Choose an option > "
  option <- getLine
  case option of
    "1" -> do
      putStr "Enter the movie title > "
      userIn <- getLine
      movs <- queryMovieByTitle conn userIn
      showMovieTitles movs
      searchMovInDB conn
    "2" -> do
      putStr "Enter the genre > "
      userIn <- getLine
      movs <- queryMovieByGenre conn userIn
      showMovieTitles movs
      searchMovInDB conn
    "3" -> do
      putStr "Enter the country of origin > "
      userIn <- getLine
      movs <- queryMovieByCountry conn userIn
      showMovieTitles movs
      searchMovInDB conn
    "4" -> do
      putStr "Enter the actor name > "
      userIn <- getLine
      movs <- queryMovieByActorName conn userIn
      showMovieTitles movs
      searchMovInDB conn
    "5" -> do
      return ()
    _ -> do
      putStrLn "Invalid option, please try again\n"
      cls
      searchMovInDB conn
  clearScreen

-- | Delete movies in the database
delMovieInDB :: Connection -> IO ()
delMovieInDB conn = do
  clearScreen
  putStrLn "Delete a movie from database"
  movs <- queryAllMovies conn
  let titleLst = map title movs
  if null movs
    then do
      putStrLn "No movie has been found."
      cls
    else do
      putStrLnListWithNumber titleLst
      putStrLn "\nType 0 to return"
      putStrLn "Type a number to select a movie or select multiple movie to delete using ',' to sepeate each number."
      putStrLn "Example: 76 or 3,10,22"
      putStr "Your choice > "
      userIn <- getLine
      let idxStrList = splitOn "," userIn
      let idxMaybes = listStringToInt idxStrList
      if all isJust idxMaybes && length idxMaybes <= length movs
        then do
          let idxs = catMaybes idxMaybes
          if length idxs == 1 && head idxs == 0
            then clearScreen
            else do
              if all (<= length movs) idxs && all (> 0) idxs
                then do
                  let removalList = map ((movs !!) . (\i -> i - 1)) idxs
                  deleteMovieByIds conn removalList
                  cls
                  delMovieInDB conn
                else do
                  putStrLn "Invalid input, please try again."
                  cls
                  delMovieInDB conn
        else do
          putStrLn "Invalid input, please try again."
          cls
          delMovieInDB conn

-- | Wipe everything in the database
wipeDatabase :: Connection -> IO ()
wipeDatabase conn = do
  putStrLn "Preparing to reset the database"
  putStr "Are you sure? [Y] Yes\t[N] No > "
  userIn <- getLine
  case userIn of
    "Y" -> resetDB conn >> putStrLn "Done." >> cls
    "N" -> putStrLn "Operation aborted." >> cls
    _ -> putStrLn "Invalid input. Operation aborted." >> cls

-- | Main menu of the app
appMain :: Connection -> IO ()
appMain conn = do
  showMenu
  option <- getLine
  case option of
    "1" -> getTopHundred conn >> appMain conn
    "2" -> searchMovieOnline conn >> appMain conn
    "3" -> searchMovInDB conn >> appMain conn
    "4" -> showAllMovieTitles conn >> appMain conn
    "5" -> dumpDBToJSON conn >> appMain conn
    "6" -> delMovieInDB conn >> appMain conn
    "7" -> wipeDatabase conn >> appMain conn
    "8" -> close conn >> putStrLn "Bye." >> exitSuccess
    _ -> putStrLn "Invalid option, please try again" >> cls >> appMain conn