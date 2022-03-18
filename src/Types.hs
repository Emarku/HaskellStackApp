{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( -- * Types

    --

    -- | This module contains all Haskell datatypes of this app
    Actor (..),
    Movie (..),
    MovieSQLite (..),
    MovActSQLite (..),
    MovGenericLinkSQLite (..),
    Details (..),
    BoxOffice (..),
    URL,
    movieToString,
  )
where

import Data.Aeson (ToJSON)
import Data.Int (Int64)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Maybe
import Data.Time (Day)
import GHC.Generics (Generic)
import MiscUtils (seperator, showFP, splitAtRecur)
import Numeric (showFFloat)
import System.Console.ANSI (getTerminalSize)

-- | URL type for URLs, such as IMDB top 250 URL
type URL = String

-- | Actor of the movie
data Actor = Actor
  { -- | Name of the actor
    name :: String,
    -- | Charactor name of the actor in the movie
    actAs :: String
  }
  deriving
    ( Show,
      -- | Derived to Generic for ToJSON
      Generic,
      -- | Derived to ToJSON for exporting datatype as JSON
      ToJSON
    )

-- | Datatype for store everying about a movie
data Movie = Movie
  { -- | ID in the database
    movieId :: Int64,
    imdbPageId :: String,
    title :: String,
    tagline :: String,
    storyline :: String,
    runtime :: String,
    -- | List of actors of the movie
    actors :: [Actor],
    genres :: String,
    directors :: String,
    writers :: String,
    details :: Details,
    boxOffice :: BoxOffice
  }
  deriving
    ( Show,
      -- | Derived to Generic for ToJSON
      Generic,
      -- | Derived to ToJSON for exporting datatype as JSON
      ToJSON
    )

-- | Misc. Details about the movie
data Details = Details
  { releaseDate :: Maybe Day,
    countries :: String,
    languages :: String,
    filmingLocation :: String,
    prodCompanies :: String
  }
  deriving
    ( Show,
      -- | Derived to Generic for ToJSON
      Generic,
      -- | Derived to ToJSON for exporting datatype as JSON
      ToJSON
    )

-- | The Box Office of the movie
data BoxOffice = BoxOffice
  { -- | The currency symbol / codes for budget
    budgetCurrency :: String,
    -- | Budget of the movie
    budget :: Maybe Float,
    -- | Gross Boxoffice United States and Canda
    grossUSC :: Maybe Float,
    -- | Gross Boxoffice World Wide
    grossWW :: Maybe Float
  }
  deriving
    ( Show,
      -- | Derived to Generic for ToJSON
      Generic,
      -- | Derived to ToJSON for exporting datatype as JSON
      ToJSON
    )

-- | Specific movie datatype for inserting / querying data from SQLite,
-- For table "Movie"
data MovieSQLite = MovieSQLite
  { -- | SQLite database ID for the movie
    movId' :: Int64,
    imdbPageId_ :: String,
    title_ :: String,
    tagline_ :: String,
    storyline_ :: String,
    runtime_ :: String,
    genres_ :: String,
    directors_ :: String,
    writers_ :: String,
    -- | Foreign key of Detail
    detailsId :: Int64,
    -- | Foreign key of BoxOffice
    boxOfficeId :: Int64
  }

-- | Specific datatype for inserting / querying data from SQLite,
-- designed to handle many to many relationship between AnyTable with Movie
-- Reserved for future use
data MovGenericLinkSQLite = MovGenericLinkSQLite
  { mglId :: Int64,
    movId :: Int64,
    linkToId :: Int64
  }

-- | Specific datatype for inserting / querying data from SQLite,
-- designed to handle many to many relationship between Movie and Actor
-- For table "MovieActor"
data MovActSQLite = MovActSQLite
  { -- | ID of MovieActor
    malId :: Int64,
    -- | Foreign key, db ID of the movie
    movId_ :: Int64,
    -- | Foreign key, db ID of the actor
    actId :: Int64,
    -- | Charactor name of the actor in the movie
    actAs_ :: String
  }

-- | A toString function for Actor data type
actorToString :: Actor -> String
actorToString a = name a ++ ", as " ++ actAs a

-- | Print a list of string in a formatted manner
prettyPrintList :: Bool -> [String] -> IO ()
prettyPrintList useTab [] = return ()
prettyPrintList useTab (sl : sls) = do
  if useTab
    then putStr $ "\t\t\t" ++ sl ++ "\n"
    else putStr $ "  " ++ sl ++ "\n"
  prettyPrintList useTab sls

-- | A formatted toString function for Movie
movieToString :: Movie -> IO ()
movieToString movie = do
  -- Get the size of the terminal, if not found, set it as common 80x24
  -- The only concern is the width here
  tSizeMaybe <- getTerminalSize
  let tCol = snd $ fromMaybe (24, 80) tSizeMaybe
  let detail = details movie
  let bo = boxOffice movie
  let acts = actors movie
  putStrLn $ seperator '-' tCol
  putStrLn $ "IMDb id:\t\t" ++ imdbPageId movie
  putStrLn $ "Title:\t\t\t" ++ title movie
  putStrLn $ "Tagline:\t\t" ++ tagline movie
  putStrLn $ "Runtime:\t\t" ++ runtime movie
  putStrLn "Storyline:"
  -- Print the storyline with 70 chars max of each line
  -- if the columns of the terminal is less than 80 then we dont use tab
  -- responsive design!
  let maxRowSL = if tCol >= 80 then 70 else tCol - 5
  prettyPrintList (maxRowSL == 70) $ splitAtRecur maxRowSL $ storyline movie
  putStrLn ""
  putStrLn "Actors:"
  prettyPrintList (maxRowSL == 70) $ map actorToString acts
  putStrLn ""
  putStrLn $ "Genres:\t\t\t" ++ genres movie
  putStrLn $ "Directors:\t\t" ++ directors movie
  putStrLn $ "Writers:\t\t" ++ writers movie
  putStr "Release Date:\t\t"
  case releaseDate detail of
    Just date -> putStr $ show date ++ "\n"
    Nothing -> putStrLn "No Data\n"
  putStrLn $ "Filming Location:\t" ++ filmingLocation detail
  putStrLn $ "Countries:\t\t" ++ countries detail
  putStrLn $ "Languages:\t\t" ++ languages detail
  putStrLn $ "Production Companies:\t" ++ prodCompanies detail
  putStr "Budget:\t\t\t"
  case budget bo of
    Just b -> putStr $ budgetCurrency bo ++ showFP b ++ "\n"
    Nothing -> putStr "No Data\n"
  putStr "Gross US & Canada:\t"
  case grossUSC bo of
    Just g -> putStr $ "$" ++ showFP g ++ "\n"
    Nothing -> putStr "No Data\n"
  putStr "Gross worldwide:\t"
  case grossWW bo of
    Just g -> putStr $ "$" ++ showFP g ++ "\n"
    Nothing -> putStr "No Data\n"
  putStrLn $ seperator '-' tCol