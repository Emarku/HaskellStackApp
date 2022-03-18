module Parse
  ( -- * Parsing

    --

    -- | This module is responsible for extracting information from pre-proccessed
    -- HTML string, parse these information to custom Haskell datatypes and exporting
    -- the data to JSON
    parseHTMLPages,
    parseHTMLPage,
    dumpAllToJSONFile,
    extractTagContent,
  )
where

import AppConfig ( defMovId )
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import MiscUtils (getUntilChar, removeUntil, removeUntilChar, stripStr)
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))
import Types
    ( BoxOffice(BoxOffice, budgetCurrency, budget, grossUSC, grossWW),
      Details(Details, releaseDate, filmingLocation, countries,
              languages, prodCompanies),
      Movie(..),
      Actor(Actor) )

-- | Generate JSON representation of the Movie datatype and save as the provided filename
dumpAllToJSONFile :: String -> [Movie] -> IO ()
dumpAllToJSONFile filename movs = writeFile filename $ BLU.toString $ encode movs

-- |  Get content inside a HTML tag
-- Note: not able to handle nested tag

-- |
-- ==== __Example Usage__
-- >>> extractTagContent ["<a href=\"/title/tt5971902/?ref_=fn_ft_tt_1\">Back for Your Life</a>"]
-- "Back for Your Life"
extractTagContent :: [String] -> String
extractTagContent [] = ""
extractTagContent (x : _) = getUntilChar '<' (removeUntilChar '>' x)

-- |  Get content inside a </meta> tag

-- |
-- ==== __Example Usage__
-- >>> extractMetaTagContent ["<meta property=\"imdb:pageConst\" content=\"tt0111161\">"]
-- "tt0111161"
extractMetaTagContent :: [String] -> String
extractMetaTagContent [] = ""
extractMetaTagContent (x : _) = do
  let spStr = splitOn " " x
  if length spStr == 3
    then getUntilChar '\"' (removeUntilChar '\"' $ last spStr)
    else ""

-- | Get the actor name and character name of a given HTML String list
extractMovieActors :: [String] -> [Actor]
extractMovieActors htmlList = do
  let cursorOne = removeUntil "data-testid=\"title-cast-item__actor\"" htmlList
  let actorName = extractTagContent cursorOne
  let cursorTwo = removeUntil "class=\"StyledComponents__CharacterNameWithoutAs" cursorOne
  let actAs = extractTagContent cursorTwo
  if null cursorOne || null cursorTwo
    then []
    else Actor actorName actAs : extractMovieActors (drop 1 cursorTwo)

-- | Get the content of a given HTML attribute that has a sequential number suffix

-- |
-- ==== __Example Usage__
-- >>> extractMovieIteratedInfo_ 1 "ref_=tt_cl_dr_" ["<a class=\"ipc-metadata-list-item__list-content-item ipc-metadata-list-item__list-content-item--link\" rel=\"\" href=\"/name/nm0751577?ref_=tt_cl_dr_1\">Anthony Russo</a>","<a class=\"ipc-metadata-list-item__list-content-item ipc-metadata-list-item__list-content-item--link\" rel=\"\" href=\"/name/nm0751648?ref_=tt_cl_dr_2\">Joe Russo</a>"]
-- ["Anthony Russo","Joe Russo"]
extractMovieIteratedInfo_ :: Int -> String -> [String] -> [String]
extractMovieIteratedInfo_ n searchStr htmlList = do
  let docCursor = removeUntil (searchStr ++ show n ++ "\">") htmlList
  let info = extractTagContent docCursor
  if null info
    then []
    else info : extractMovieIteratedInfo_ (n + 1) searchStr (drop 1 docCursor)

-- | extractMovieIteratedInfo_ with starting n = 1
extractMovieIteratedInfo :: String -> [String] -> [String]
extractMovieIteratedInfo = extractMovieIteratedInfo_ 1

-- | Generic function to extract HTML contents inside a tag with specific search string
extractMovieInfo :: [Char] -> [String] -> [String]
extractMovieInfo searchStr htmlList = do
  let docCursor = removeUntil searchStr htmlList
  let result = extractTagContent docCursor
  if null result
    then []
    else result : extractMovieInfo searchStr (drop 1 docCursor)

-- | Generic function to extract HTML contents after n lines
-- inside a tag with specific search string
extractMovieInfoSkipLines :: Int -> [Char] -> [String] -> String
extractMovieInfoSkipLines n searchStr htmlList = do
  let docCursor = removeUntil searchStr htmlList
  if null docCursor && length docCursor < n
    then ""
    else extractTagContent [docCursor !! n]

-- | Handle the case of extracting the storyline, as for some storyline string,
-- there is <a> tag inside
extractMovieStoryline :: [String] -> String
extractMovieStoryline htmlList = do
  let cursorOne = removeUntil "data-testid=\"storyline-plot-summary\"" htmlList
  if null cursorOne
    then ""
    else do
      let storylineStr = cursorOne !! 2
      let strOne = extractTagContent [storylineStr]

      -- use regex to handle the case that there is <a> inside storyline
      let regex = "<a class=.*?>([\\w\\d\\s]+)</a>"
      let result = storylineStr =~ regex :: [[String]]
      let aTag = if null result then "" else last (last result)

      if null aTag
        then strOne
        else do
          -- The last element is the string that we want
          let cursorTwo = last $ splitOn aTag storylineStr
          let strTwo = extractTagContent [cursorTwo]
          -- concat everything for the returning string
          strOne ++ aTag ++ strTwo

-- | Handle the case of extracting movie runtime, as there is <! ->
-- between the content
extractMovieRuntime :: [String] -> String
extractMovieRuntime htmlList = do
  let docCursor = removeUntil ">Runtime<" htmlList
  if null docCursor
    then ""
    else do
      let rawRTStr = getUntilChar '/' (removeUntilChar '>' (docCursor !! 1))
      stripStr "<! ->" rawRTStr

-- | Extracting the IMDb id of the mobvie from meta tag
extractIMDbId :: [String] -> String
extractIMDbId htmlList = do
  let docCursor = removeUntil "meta property=\"imdb:pageConst\"" htmlList
  if null docCursor
    then ""
    else extractMetaTagContent docCursor

-- | The main function to parse the HTML string to Movie datatype
parseHTMLPage :: String -> IO Movie
parseHTMLPage page = do
  -- As lines will not work here, we will split the html page string with '><'
  let p = splitOn "><" page

  -- Regex expression for box office
  let regex = "\\d+"

  -- Extracting filming location
  let fLoc = extractMovieInfo "ref_=tt_dt_loc\">" p

  -- Extracting BoxOffice, budget
  -- Some budget string may contain &nbsp (\xa0) as space character
  -- which is different from the usual space character, between
  -- currency and actual number, handle this here
  let budgetStr = splitOn "\xa0" $ extractMovieInfoSkipLines 4 ">Budget<" p

  -- Regex.PCRE has some problem on handling String with UTF-8 chars (i.e. currency symbol)
  -- (which is fine with UTF8.ByteString but not String) Remove it before parsing
  let budget = if length budgetStr > 1 then last budgetStr else drop 1 $ last budgetStr

  -- Extracting BoxOffice, gross US and Canada
  let grossUSC = extractMovieInfoSkipLines 4 ">Gross US & Canada<" p
  -- Extracting BoxOffice, gross WW
  let grossWW = extractMovieInfoSkipLines 4 ">Gross worldwide<" p
  -- Extracting release date
  let releaseDateStr = getUntilChar '(' (head (extractMovieInfo "releaseinfo?ref_=tt_dt_rdat\">" p))

  let boxOffice =
        BoxOffice
          { budgetCurrency = if length budgetStr > 1 then head budgetStr else take 1 $ head budgetStr,
            budget = readMaybe (concat $ concat (budget =~ regex :: [[String]])) :: Maybe Float,
            grossUSC = readMaybe (concat $ concat (grossUSC =~ regex :: [[String]])) :: Maybe Float,
            grossWW = readMaybe (concat $ concat (grossWW =~ regex :: [[String]])) :: Maybe Float
          }

  let details =
        Details
          { -- parsing the string with corresponding data format
            releaseDate = parseTimeM True defaultTimeLocale "%B %e, %Y" releaseDateStr :: Maybe Day,
            filmingLocation = if null fLoc then "" else head fLoc,
            countries = intercalate ", " $ extractMovieInfo "href=\"/search/title/?country_of_origin=" p,
            languages = intercalate ", " $ extractMovieInfo "href=\"/search/title?title_type=feature&primary_language=" p,
            prodCompanies = intercalate ", " $ extractMovieIteratedInfo "ref_=tt_dt_co_" p
          }

  -- Assemble everything and return it
  let movie =
        Movie
          { movieId = defMovId,
            imdbPageId = extractIMDbId p,
            title = head (extractMovieInfo "data-testid=\"hero-title-block__title\"" p),
            tagline = extractMovieInfoSkipLines 5 ">Taglines<" p,
            storyline = extractMovieStoryline p,
            runtime = extractMovieRuntime p,
            actors = extractMovieActors p,
            boxOffice = boxOffice,
            genres = intercalate ", " $ extractMovieInfo "genres&ref_=tt_ov_inf\">" p,
            details = details,
            directors = intercalate ", " $ extractMovieIteratedInfo "ref_=tt_cl_dr_" p,
            writers = intercalate ", " $ extractMovieIteratedInfo "ref_=tt_cl_wr_" p
          }
  return movie

-- | Batch parsing html pages
parseHTMLPages :: [String] -> IO [Movie]
parseHTMLPages [] = return []
parseHTMLPages (page : pages) = do
  mov <- parseHTMLPage page
  movList <- parseHTMLPages pages
  return $ mov : movList
