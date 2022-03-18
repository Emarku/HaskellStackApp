{-# LANGUAGE OverloadedStrings #-}

module Queries where

import Database.SQLite.Simple (Query)

-- * Database Queries

--

-- $doc This module contains all query strings for the database

-- * For database initalisation

-- | Turn on foreign key support
-- for 'ON DELETE CASCADE'
fkOn :: Query
fkOn = "PRAGMA foreign_keys = ON;"

createTableDetails :: Query
createTableDetails =
  "CREATE TABLE IF NOT EXISTS Details( \
  \id INTEGER PRIMARY KEY,\
  \rel_date TEXT DEFAULT NULL,\
  \countries TEXT NOT NULL,\
  \languages TEXT NOT NULL,\
  \filming_loc TEXT NOT NULL,\
  \prod_comp TEXT NOT NULL\
  \);"

createTableBoxOffice :: Query
createTableBoxOffice =
  "CREATE TABLE IF NOT EXISTS BoxOffice (\
  \id INTEGER PRIMARY KEY,\
  \budget_curr VARCHAR(3) DEFAULT NULL,\
  \budget REAL DEFAULT NULL,\
  \gross_usc REAL DEFAULT NULL,\
  \gross_ww REAL DEFAULT NULL\
  \);"

createTableActor :: Query
createTableActor =
  "CREATE TABLE IF NOT EXISTS Actor (\
  \id INTEGER PRIMARY KEY,\
  \name TEXT NOT NULL\
  \);"

createTableMovie :: Query
createTableMovie =
  "CREATE TABLE IF NOT EXISTS Movie (\
  \id INTEGER PRIMARY KEY,\
  \imdb_id TEXT NOT NULL,\
  \title TEXT NOT NULL,\
  \tagline TEXT NOT NULL,\
  \storyline TEXT NOT NULL,\
  \runtime TEXT NOT NULL,\
  \genres TEXT NOT NULL,\
  \directors TEXT NOT NULL,\
  \writers TEXT NOT NULL,\
  \detail INTEGER NOT NULL,\
  \box_office INTEGER NOT NULL,\
  \FOREIGN KEY(detail) REFERENCES Details(id) ON DELETE CASCADE,\
  \FOREIGN KEY(box_office) REFERENCES BoxOffice(id) ON DELETE CASCADE\
  \);"

createTableMovieActor :: Query
createTableMovieActor =
  "CREATE TABLE IF NOT EXISTS MovieActor (\
  \id INTEGER PRIMARY KEY,\
  \mov_id INTEGER NOT NULL,\
  \act_id INTEGER NOT NULL,\
  \act_as TEXT NOT NULL,\
  \FOREIGN KEY(mov_id) REFERENCES Movie(id) ON DELETE CASCADE,\
  \FOREIGN KEY(act_id) REFERENCES Actor(id) ON DELETE CASCADE\
  \);"

-- * For inserting new row to database

insertMovieDetail :: Query
insertMovieDetail = "INSERT INTO Details (rel_date, countries, languages, filming_loc, prod_comp) VALUES (?,?,?,?,?)"

insertBoxOffice :: Query
insertBoxOffice = "INSERT INTO BoxOffice (budget_curr, budget, gross_usc, gross_ww) VALUES (?,?,?,?)"

insertActor :: Query
insertActor = "INSERT INTO Actor (name) VALUES (?)"

insertMovie :: Query
insertMovie = "INSERT INTO Movie (title, imdb_id, tagline, storyline, runtime, genres, directors, writers, detail, box_office) VALUES (?,?,?,?,?,?,?,?,?,?)"

insertMovieActor :: Query
insertMovieActor = "INSERT INTO MovieActor (mov_id, act_id, act_as) VALUES (?,?,?)"

-- * Existence check queries

checkMovie :: Query
checkMovie = "SELECT id FROM Movie WHERE imdb_id = ?"

checkActor :: Query
checkActor = "SELECT id FROM Actor WHERE name = ?"

-- * Drop tables

dropDetails :: Query
dropDetails = "DROP TABLE IF EXISTS Details"

dropBoxOffice :: Query
dropBoxOffice = "DROP TABLE IF EXISTS BoxOffice"

dropActor :: Query
dropActor = "DROP TABLE IF EXISTS Actor"

dropMovie :: Query
dropMovie = "DROP TABLE IF EXISTS Movie"

dropMovieActor :: Query
dropMovieActor = "DROP TABLE IF EXISTS MovieActor"

-- * User queries

queryDetailsById :: Query
queryDetailsById = "SELECT rel_date, countries, languages, filming_loc, prod_comp FROM Details WHERE id = ?"

queryBoxOfficeById :: Query
queryBoxOfficeById = "SELECT budget_curr, budget, gross_usc, gross_ww FROM BoxOffice WHERE id = ?"

queryMovActByMovId :: Query
queryMovActByMovId = "SELECT * FROM MovieActor WHERE mov_id = ?"

queryMovIdByActId :: Query
queryMovIdByActId = "SELECT mov_id FROM MovieActor WHERE act_id = ?"

queryActorById :: Query
queryActorById = "SELECT name FROM Actor WHERE id = ?"

queryActorByName :: Query
queryActorByName = "SELECT id FROM Actor WHERE UPPER(name) = UPPER(?)"

queryAllMov :: Query
queryAllMov = "SELECT * from Movie"

queryMovieById :: Query
queryMovieById = "SELECT * from Movie WHERE id = ?"

queryMovieByDetailsId :: Query
queryMovieByDetailsId = "SELECT * from Movie WHERE detail = ?"

queryMovByTitle :: Query
queryMovByTitle = "SELECT * from Movie WHERE title LIKE ?"

queryMovByGenre :: Query
queryMovByGenre = "SELECT * from Movie WHERE genres LIKE ?"

queryDetailsByCountry :: Query
queryDetailsByCountry = "SELECT id from Details WHERE countries LIKE ?"

delMovById :: Query
delMovById = "DELETE FROM Movie WHERE id = ?"