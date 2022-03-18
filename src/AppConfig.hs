module AppConfig where
import Types (URL)
import Data.Int (Int64)

-- * App Configuration

--

-- $doc This module contains the configuration for the entire app,
-- e.g. the URL of the website and the filename of the database

-- | Base URL of the IMDB site
baseURL :: URL
baseURL = "https://www.imdb.com/"

-- | URL of the top 250 movie IMDB page
imdbTopUrl :: URL
imdbTopUrl = baseURL ++ "chart/top/"

-- | Base URL of the search function
imdbSearchUrl :: URL
imdbSearchUrl = baseURL ++ "find?q="

-- | Query string for the search function
-- (Search category: Movie Titles)
imdbSearchCat :: String
imdbSearchCat = "&s=tt&ttype=ft&ref_=fn_ft"

-- | SQLite database filename
dbFileName :: String
dbFileName = "movie.sqlite"

-- | Default movieId
defMovId :: Int64
defMovId = -1