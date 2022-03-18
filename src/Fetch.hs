{-# LANGUAGE OverloadedStrings #-}

module Fetch
  ( -- * Fetching

    --

    -- | This module contains functions to get web page(s) from the internet
    -- and some basic extraction functions to filter out unused information
    -- before passing to parsing module
    download,
    downloadList,
    getAllMovieURL,
    getSearchResult,
  )
where

import AppConfig (baseURL, imdbTopUrl)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List.Split (splitOn)
import qualified Data.Text as TS
import qualified Data.Text.Internal.Builder as TB
import qualified Data.Text.Lazy as TL
import HTMLEntities.Decoder (htmlEncodedText)
import MiscUtils
  ( getUntilChar,
    removeUntil,
    removeUntilChar,
    trimList,
  )
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    parseRequest,
  )
import Parse (extractTagContent)
import Types (URL)

-- | Get a page with a given URL, pack the response UTF8 ByteString as text,
-- pass it to HTML decoder to decode all HTML escaped characters and
-- unpack as String to return the resulting HTML
download :: URL -> IO String
download url = do
  -- Make a request using the given URL
  request <- parseRequest url
  response <- httpLBS request
  -- Convert response ByteString to Text
  let pageText = TS.pack $ BLU.toString $ getResponseBody response
  -- Decode the HTML escaped string
  let decodedText = htmlEncodedText pageText
  -- As decodedText is a Builder, we need to make it as Text
  -- and convert it back to String
  let pageStr = TL.unpack $ TB.toLazyText decodedText
  return pageStr

-- | Batch get pages with URLs
downloadList :: [URL] -> IO [String]
downloadList [] = return []
downloadList (url : urls) = do
  page <- download (baseURL ++ url)
  pageList <- downloadList urls
  return $ page : pageList

-- | Take the first element of an splitted HTML String list,
-- remove '<a href="' and '"'
getURL :: [String] -> String
getURL [] = []
getURL (x : _) = init (drop 9 x)

-- | Extract URLs of each movie title from the 
-- Take a splitted HTML String list as input, 
-- get the "a href" URL inside titleColumn
extractAllMovieURL :: [String] -> [String]
extractAllMovieURL [] = []
extractAllMovieURL htmlList = do
  -- Find titleColumn and '<a href=', example result head of the list
  -- "<a href="/title/tt2278388/?pf_rd_m=A2FGELUUNOQJNL&amp;pf_rd_p=9703a62d-b88a-4e30-ae12-90fcafafa3fc&amp;
  -- pf_rd_r=WAGYEJSFRZ4E7AYV2AE3&amp;pf_rd_s=center-1&amp;pf_rd_t=15506&amp;pf_rd_i=top&amp;ref_=chttp_tt_189"
  -- title="Wes Anderson (dir.), Ralph Fiennes, F. Murray Abraham">The Grand Budapest Hotel</a>"
  let htmlStr = removeUntil "<a href=\"" (removeUntil "<td class=\"titleColumn\">" htmlList)
  let urlString = getURL htmlStr
  -- Expected result example:
  -- /title/tt2278388/?pf_rd_m=A2FGELUUNOQJNL&amp;pf_rd_p=9703a62d-b88a-4e30-ae12-90fcafafa3fc&amp;
  -- pf_rd_r=WAGYEJSFRZ4E7AYV2AE3&amp;pf_rd_s=center-1&amp;pf_rd_t=15506&amp;pf_rd_i=top&amp;ref_=chttp_tt_189
  if null urlString
    then []
    else urlString : extractAllMovieURL (drop 1 htmlStr)

-- | Pre-processing function for extractAllMovieURL
-- Split the page string and remove all unused information
getAllMovieURL :: String -> [String]
getAllMovieURL htmlRaw = do
  let htmlLines = lines htmlRaw
  let content = trimList (reverse (removeUntil "</tbody>" (reverse (removeUntil "<tbody class=\"lister-list\">" htmlLines))))
  extractAllMovieURL content

-- | Extract URLs and title of the search result page
-- Take a splitted string list as input,
-- get the "a href" URL and content text of a tag inside result_text
-- return a tuple which the first element is movie title and the second element is
-- the url of the movie
extractAllSearchResult :: [String] -> [(String, String)]
extractAllSearchResult [] = []
extractAllSearchResult htmlList = do
  -- Find result_text and 'a href=', example result head of the list
  -- "<a href="/title/tt5971902/?ref_=fn_ft_tt_1">Back for Your Life</a>""
  let htmlStr = removeUntil "a href=\"" (removeUntil "td class=\"result_text\"" htmlList)
  -- Expected result example: "/title/tt5971902/?ref_=fn_ft_tt_1"
  let urlString = getUntilChar '\"' $ getURL htmlStr
  -- Expected result example: "Back for Your Life"
  let titleString = extractTagContent $ take 1 htmlStr
  if null urlString || null titleString
    then []
    else (titleString, urlString) : extractAllSearchResult (drop 1 htmlStr)

-- | Pre-processing function for extractAllSearchResult
-- Split the page string and get the search result part of the html,
-- split the resulting long html string again with "> <"
-- for further processing
getSearchResult :: String -> [(String, String)]
getSearchResult htmlRaw = do
  let htmlLines = lines htmlRaw
  let content = removeUntil "<table class=\"findList\">" htmlLines !! 1
  extractAllSearchResult $ splitOn "> <" content