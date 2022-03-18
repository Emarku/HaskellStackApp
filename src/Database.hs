{-# LANGUAGE OverloadedStrings #-}

module Database
  ( -- * Database

    --

    -- | This module is responsible for handling the initalisation, insertion and deletion of data and
    -- queries to the database
    initDB,
    resetDB,
    beginInsertMovieTransactions,
    queryAllMovies,
    queryMovieByTitle,
    queryMovieByGenre,
    queryMovieByCountry,
    queryMovieByActorName,
    deleteMovieByIds,
  )
where

import AppConfig (dbFileName)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Typeable (typeOf)
import Database.SQLite.Simple
import qualified Database.SQLite.Simple.ToField as DS
import Queries
import Types

instance ToRow Details where
  toRow (Details releaseDate countries languages filmingLocation prodCompanies) =
    toRow (releaseDate, countries, languages, filmingLocation, prodCompanies)

instance ToRow BoxOffice where
  toRow (BoxOffice budgetCurrency budget grossUSC grossWW) =
    toRow (budgetCurrency, budget, grossUSC, grossWW)

instance ToRow Actor where
  toRow (Actor name actAs) = toRow (Only name)

instance ToRow MovieSQLite where
  toRow (MovieSQLite movId' imdbPageId_ title_ tagline_ storyline_ runtime_ genres_ directors_ writers_ detailId boxOfficeId) =
    toRow (title_, imdbPageId_, tagline_, storyline_, runtime_, genres_, directors_, writers_, detailId, boxOfficeId)

instance ToRow MovActSQLite where
  toRow (MovActSQLite malId movId_ actId actAs_) = toRow (movId_, actId, actAs_)

instance ToRow MovGenericLinkSQLite where
  toRow (MovGenericLinkSQLite mglId movId linkToId) = toRow (movId, linkToId)

instance FromRow Details where
  fromRow = Details <$> field <*> field <*> field <*> field <*> field

instance FromRow BoxOffice where
  fromRow = BoxOffice <$> field <*> field <*> field <*> field

instance FromRow MovActSQLite where
  fromRow = MovActSQLite <$> field <*> field <*> field <*> field

instance FromRow MovieSQLite where
  fromRow = MovieSQLite <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- | Fully wipe the database
-- Drop all tables in the database
dropAllTables :: Connection -> IO ()
dropAllTables conn = do
  execute_ conn dropMovieActor
  execute_ conn dropMovie
  execute_ conn dropDetails
  execute_ conn dropBoxOffice
  execute_ conn dropActor

-- | Create all tables for this app
createAllTables :: Connection -> IO ()
createAllTables conn = do
  execute_ conn createTableDetails
  execute_ conn createTableBoxOffice
  execute_ conn createTableActor
  execute_ conn createTableMovie
  execute_ conn createTableMovieActor

-- | Initalisation of database
-- Enable foreign key support and create tables if they are not exist
initDB :: IO Connection
initDB = do
  conn <- open dbFileName
  execute_ conn fkOn
  createAllTables conn
  return conn

-- | Drop and recreate all tables
resetDB :: Connection -> IO ()
resetDB conn = do
  dropAllTables conn
  createAllTables conn

-- |  Make a single-value "collection" list
toOnly :: [a] -> [Only a]
toOnly = map Only

-- | Make a string for pattern matching search with "LIKE" operator
makeLikeStr :: String -> String
makeLikeStr str = "%" ++ str ++ "%"

-- | A generic function to check if the given information exists in DB
-- with the supplied query string and target string.
-- If yes, we will return the row id, return -1 if that is not exist
existenceCheck :: ToRow q => Connection -> q -> Query -> IO Int64
existenceCheck conn obj q = do
  result <- query conn q obj :: IO [[Int64]]
  if null result
    then return $ -1
    else return $ head $ concat result

-- | Insert a row to DB with the supplied query and datatype
-- and return the row id
genericDBInsertReturnId :: ToRow q => Connection -> q -> Query -> IO Int64
genericDBInsertReturnId conn obj q = do
  execute conn q obj
  lastInsertRowId conn

-- | Check existence before insert a row to DB
-- If exists, return the Id, do the insertion if no
genericDBInsertWithCheckReturnId :: (ToRow q1, ToRow q2) => Connection -> q1 -> q2 -> Query -> Query -> IO Int64
genericDBInsertWithCheckReturnId conn obj checkStr checkQ insertQ = do
  existence <- existenceCheck conn checkStr checkQ
  if existence == -1
    then genericDBInsertReturnId conn obj insertQ
    else return existence

-- | Batch processing of genericDBInsertWithCheckReturnId
genericDBInsertWithCheckReturnIdList :: (ToRow q1, ToRow q2) => Connection -> [q1] -> [q2] -> Query -> Query -> IO [Int64]
genericDBInsertWithCheckReturnIdList conn [] _ checkQ insertQ = return []
genericDBInsertWithCheckReturnIdList conn _ [] checkQ insertQ = return []
genericDBInsertWithCheckReturnIdList conn (obj : objList) (cs : csList) checkQ insertQ = do
  objId <- genericDBInsertWithCheckReturnId conn obj cs checkQ insertQ
  objIdList <- genericDBInsertWithCheckReturnIdList conn objList csList checkQ insertQ
  return $ objId : objIdList

-- | Insert a new row into intermediate table between many to many relationship,
-- designed to handle Actors-Movies or other many-many relationship.
-- Requires Movie Row id and a corresponding table row id.
-- Actors-Movies are required to have a tuple list input
genericDBManyToManyLinkInsert :: Connection -> Int64 -> Either [(Int64, Actor)] [Int64] -> Query -> IO ()
genericDBManyToManyLinkInsert conn mId (Left []) q = return ()
genericDBManyToManyLinkInsert conn mId (Right []) q = return ()
genericDBManyToManyLinkInsert conn mId (Left (obj : objList)) q = do
  execute conn q $ MovActSQLite (-1) mId (fst obj) (actAs $ snd obj)
  genericDBManyToManyLinkInsert conn mId (Left objList) q
genericDBManyToManyLinkInsert conn mId (Right (obj : objList)) q = do
  execute conn q $ MovGenericLinkSQLite (-1) mId obj
  genericDBManyToManyLinkInsert conn mId (Right objList) q

-- | Main function to insert a new movie to the database,
-- disassemble Movie datatype to insert to various tables
insertNewMovie :: Connection -> Movie -> IO ()
insertNewMovie conn movie = do
  dId <- genericDBInsertReturnId conn (details movie) insertMovieDetail
  boId <- genericDBInsertReturnId conn (boxOffice movie) insertBoxOffice
  aIdList <- genericDBInsertWithCheckReturnIdList conn (actors movie) (toOnly (map name $ actors movie)) checkActor insertActor
  let mov =
        MovieSQLite
          { movId' = -1,
            imdbPageId_ = imdbPageId movie,
            title_ = title movie,
            tagline_ = tagline movie,
            storyline_ = storyline movie,
            runtime_ = runtime movie,
            genres_ = genres movie,
            directors_ = directors movie,
            writers_ = writers movie,
            detailsId = dId,
            boxOfficeId = boId
          }
  movId <- genericDBInsertReturnId conn mov insertMovie
  genericDBManyToManyLinkInsert conn movId (Left (zip aIdList $ actors movie)) insertMovieActor

-- | Do each movie insertion as a transaction.
-- If anything did wrong in the insertion process, we revert all operations
beginInsertMovieTransactions :: Connection -> [Movie] -> IO ()
beginInsertMovieTransactions conn [] = return ()
beginInsertMovieTransactions conn (mov : movList) = do
  movExist <- existenceCheck conn (Only $ imdbPageId mov) checkMovie
  if movExist == -1
    then withTransaction conn (insertNewMovie conn mov)
    else putStrLn $ "Movie \"" ++ title mov ++ "\" exists, skip."
  beginInsertMovieTransactions conn movList

-- | Batch query on the row id on a given query string
-- and row id list on other tables
genericQueryByIds :: (ToRow q, FromRow r) => Connection -> Query -> [q] -> IO [[r]]
genericQueryByIds conn q [] = return []
genericQueryByIds conn q (sqlId : sqlIds) = do
  result <- query conn q sqlId
  resultList <- genericQueryByIds conn q sqlIds
  return $ result : resultList

-- Assemble actors list using the data from the database
assembleActor :: [String] -> [String] -> [Actor]
assembleActor [] _ = []
assembleActor _ [] = []
assembleActor (an : actName) (aaa : actActAs) = Actor an aaa : assembleActor actName actActAs

-- Assemble movies list using the data from the database
assembleMovies :: [MovieSQLite] -> [Details] -> [BoxOffice] -> [[Actor]] -> [Movie]
assembleMovies [] _ _ _ = []
assembleMovies _ [] _ _ = []
assembleMovies _ _ [] _ = []
assembleMovies _ _ _ [] = []
assembleMovies (msq : msqs) (d : ds) (b : bs) (a : as) =
  Movie
    { movieId = movId' msq,
      imdbPageId = imdbPageId_ msq,
      title = title_ msq,
      tagline = tagline_ msq,
      storyline = storyline_ msq,
      runtime = runtime_ msq,
      actors = a,
      boxOffice = b,
      genres = genres_ msq,
      details = d,
      directors = directors_ msq,
      writers = writers_ msq
    } :
  assembleMovies msqs ds bs as

-- | Return the actors of a given movie
-- query using the actor id
queryActorsOfMovies :: Connection -> [[MovActSQLite]] -> IO [[Actor]]
queryActorsOfMovies conn [] = return []
queryActorsOfMovies conn (ma : mas) = do
  let actActAs = map actAs_ ma
  actName <- genericQueryByIds conn queryActorById (toOnly (map actId ma)) :: IO [[[String]]]
  let actorList = assembleActor (concat $ concat actName) actActAs
  aom <- queryActorsOfMovies conn mas
  return $ actorList : aom

-- | Main function to Query all tables that is related to movie
-- and assemble the result to a list of movies
queryLinkedTablesAndAssemble :: Connection -> [MovieSQLite] -> IO [Movie]
queryLinkedTablesAndAssemble conn msqs = do
  let detailsSQLiteId = map detailsId msqs
  let boSQLiteId = map boxOfficeId msqs
  let movSQLiteId = map movId' msqs
  detailsResult <- genericQueryByIds conn queryDetailsById $ toOnly detailsSQLiteId :: IO [[Details]]
  boResult <- genericQueryByIds conn queryBoxOfficeById $ toOnly boSQLiteId :: IO [[BoxOffice]]
  movAct <- genericQueryByIds conn queryMovActByMovId $ toOnly movSQLiteId :: IO [[MovActSQLite]]
  actorsList <- queryActorsOfMovies conn movAct
  return $ assembleMovies msqs (concat detailsResult) (concat boResult) actorsList

-- | A generic function to make query on the movie table
genericMovieQuery :: ToRow q => Connection -> Query -> q -> IO [Movie]
genericMovieQuery conn q values = do
  msqs <- query conn q values :: IO [MovieSQLite]
  queryLinkedTablesAndAssemble conn msqs

-- | Returning all movies in the database
queryAllMovies :: Connection -> IO [Movie]
queryAllMovies conn = do
  msqs <- query_ conn queryAllMov :: IO [MovieSQLite]
  queryLinkedTablesAndAssemble conn msqs

-- | Returning all movies that the title contains user entered keyword (case insensitive)
queryMovieByTitle :: Connection -> String -> IO [Movie]
queryMovieByTitle conn str = genericMovieQuery conn queryMovByTitle (Only $ makeLikeStr str)

-- | Returning all movies that the genre contains user entered keyword (caseless)
queryMovieByGenre :: Connection -> String -> IO [Movie]
queryMovieByGenre conn str = genericMovieQuery conn queryMovByGenre (Only $ makeLikeStr str)

-- | Returning all movies that country of origin contains user entered keyword (case insensitive)
queryMovieByCountry :: Connection -> String -> IO [Movie]
queryMovieByCountry conn str = do
  detailsSQLiteId <- query conn queryDetailsByCountry (Only $ makeLikeStr str) :: IO [[Int64]]
  msqs <- genericQueryByIds conn queryMovieByDetailsId $ toOnly $ concat detailsSQLiteId :: IO [[MovieSQLite]]
  queryLinkedTablesAndAssemble conn $ concat msqs

-- | Returning all movies that has the user inputted actor name (case insensitive)
queryMovieByActorName :: Connection -> String -> IO [Movie]
queryMovieByActorName conn str = do
  actIds <- query conn queryActorByName (Only str) :: IO [[Int64]]
  movIds <- genericQueryByIds conn queryMovIdByActId $ toOnly $ concat actIds :: IO [[[Int64]]]
  msqs <- genericQueryByIds conn queryMovieById $ toOnly $ concat $ concat movIds :: IO [[MovieSQLite]]
  queryLinkedTablesAndAssemble conn $ concat msqs

-- | Delete a movie from data with the id
deleteMovieByIds :: Connection -> [Movie] -> IO ()
deleteMovieByIds conn [] = return ()
deleteMovieByIds conn (mov : movs) = do
  execute conn delMovById (Only $ movieId mov)
  putStrLn $ "Movie \"" ++ title mov ++ "\" is now deleted."
  deleteMovieByIds conn movs
