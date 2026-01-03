{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Control.Concurrent (threadDelay)
import Control.Exception.Safe (handleAny)
import Control.Monad (forM_, void, when)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON)
import Data.ByteString.Char8 (pack)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (fromList, toList)
import GHC.Generics ( Generic )
import Network.Algolia.Search hiding (length)
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)
import System.ProgressBar
import Test.WebDriver (Capabilities(browser), WDConfig(..), chrome, chromeOptions, closeSession,
  defaultCaps, defaultConfig, getSource, openPage, runSession)
import Text.HTML.Scalpel
import Text.Regex.Base.RegexLike (makeRegex)
import Text.Regex.Posix (Regex)
import Text.StringLike (StringLike(toString))

data Slide = Slide
  { header :: String
  , body :: String
  } deriving Show

data AlgoliaRecord = AlgoliaRecord
  { title :: String
  , logo :: URL
  , permalink :: URL
  , section :: String
  , content :: String
  } deriving (Generic, Show)

instance ToJSON AlgoliaRecord

chromeConfig :: WDConfig
chromeConfig = defaultConfig
    { wdHost = "localhost"
    , wdPort = 4444
    , wdCapabilities = defaultCaps {
        browser = chrome {
          chromeOptions =
            [
              "--no-sandbox",
              "--disable-dev-shm-usage",
              "--disable-blink-features=AutomationControlled",
              "--start-maximized",
              "--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
            ]
        }
      }
    }

startOverIndex :: IndexName a
startOverIndex = IndexName "start-over"

main :: IO ()
main = do
  algoliaClient <-
    mkAlgoliaClient
      <$> (ApiKey . pack <$> getEnv "API_KEY")
      <*> (ApplicationId . pack <$> getEnv "APPLICATION_ID")

  simpleAlgolia algoliaClient $ clearIndex startOverIndex

  putStrLn "Fetching sites from Spaceport"
  sites <- fetchSitesFromSpaceport

  progressBar <- newProgressBar
    progressBarStyle
    10
    (Progress 0 (length sites) ())

  forM_ sites $ \site -> do
    records <- processSite site
    simpleAlgolia algoliaClient $
      mapM_ (\a -> catch (void $ addObjectWithoutId startOverIndex a) (\(_::AlgoliaError) -> lift $ putStrLn "Failed to add record")) records
    incProgress progressBar 1

  where
    fetchSitesFromSpaceport :: IO [URL]
    fetchSitesFromSpaceport =
      Set.toList . Set.fromList . fmap normalise . fromMaybe [] <$>
        scrapeURL' "https://spaceport.mystrikingly.com" 30000000 urls

    normalise :: URL -> URL
    normalise = removeHttps . removeTrailingSlash

    removeHttps :: URL -> URL
    removeHttps url = case stripPrefix "https" url of
      Nothing -> url
      Just rest -> "http" <> rest

    removeTrailingSlash :: URL -> URL
    removeTrailingSlash url = case last url of
      '/' -> init url
      _   -> url

    urls :: Scraper URL [URL]
    urls = attrs "href" $ "a" @: ["href" @=~ strikinglyRegex]

    strikinglyRegex :: Regex
    strikinglyRegex = makeRegex
      ("http[s]?:\\/\\/.*\\.mystrikingly\\.com" :: String)

    progressBarStyle :: Style s
    progressBarStyle = defStyle
      { stylePrefix = msg "Indexing"
      , stylePostfix = exact
      }

scrapeURL' :: URL -> Int -> Scraper String a -> IO (Maybe a)
scrapeURL' url timeout scraper =  do
  htmlSource <- runSession chromeConfig $ do
    openPage url
    -- Wait for the WAF challenge to finish
    liftIO $ threadDelay timeout -- 30 seconds
    -- Get the rendered HTML source
    htmlSource <- getSource
    closeSession
    pure htmlSource

  pure $ scrapeStringLike (toString htmlSource) scraper

processSite :: URL -> IO [AlgoliaRecord]
processSite site = handleAny handleException . fmap (fromMaybe []) . scrapeURL' site 15000000 $ do
    title <- attr "content" $ "meta" @: ["property" @= "og:site_name"]
    logo <- attr "src" $ "div" @: [hasClass "s-logo"] // "img"
    chroots ("li" @: [hasClass "slide"]) (slide title logo site)
  where
    handleException _ = do
        hPutStrLn stderr $ "Failed to process site " <> site
        pure []

    slide :: String -> URL -> URL -> Scraper String AlgoliaRecord
    slide title logo site = do
      sectionId <- attr "id" $ "li" @: [hasClass "slide"]
      section <- text $ "div" @: [hasClass "s-title"] // "h2"
      content <- fmap unwords . texts $
        "div" @: [hasClass "s-item-text-group"]
      when (null content) (fail "Empty content")
      pure $ AlgoliaRecord
        { title
        , logo
        , permalink = site <> "#" <> sectionId
        , section
        , content
        }
