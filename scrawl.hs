{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Control.Monad (void, when)
import Control.Monad.Catch (catch)
import Data.Aeson (ToJSON)
import Data.ByteString.Char8 (pack)
import Data.Foldable (for_)
import Data.List (intercalate, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (fromList, toList)
import GHC.Generics ( Generic )
import Network.Algolia.Search hiding (length)
import System.Environment (getEnv)
import System.ProgressBar
import Text.HTML.Scalpel
import Text.Regex.Base.RegexLike (makeRegex)
import Text.Regex.Posix (Regex)

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

  for_ sites $ \site -> do
    records <- processSite site
    simpleAlgolia algoliaClient $
      mapM_ (\a -> catch (void $ addObjectWithoutId startOverIndex a) (\(_::AlgoliaError) -> pure ())) records
    incProgress progressBar 1

  where
    fetchSitesFromSpaceport :: IO [URL]
    fetchSitesFromSpaceport =
      Set.toList . Set.fromList . fmap normalise . fromMaybe [] <$>
        scrapeURL "https://spaceport.mystrikingly.com" urls

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

    urls :: Scraper String [URL]
    urls = attrs "href" $ "a" @: ["href" @=~ strikinglyRegex]

    strikinglyRegex :: Regex
    strikinglyRegex = makeRegex
      ("http[s]?:\\/\\/.*\\.mystrikingly\\.com" :: String)

    progressBarStyle :: Style s
    progressBarStyle = defStyle
      { stylePrefix = msg "Indexing"
      , stylePostfix = exact
      }

processSite :: URL -> IO [AlgoliaRecord]
processSite site = fmap (fromMaybe []) . scrapeURL site $ do
    title <- attr "content" $ "meta" @: ["property" @= "og:site_name"]
    logo <- attr "src" $ "div" @: [hasClass "s-logo"] // "img"
    chroots ("li" @: [hasClass "slide"]) (slide title logo site)
  where
    slide :: String -> URL -> URL -> Scraper String AlgoliaRecord
    slide title logo site = do
      sectionId <- attr "id" $ "li" @: [hasClass "slide"]
      section <- text $ "div" @: [hasClass "s-title"] // "h2"
      content <- fmap (intercalate " ") . texts $
        "div" @: [hasClass "s-item-text-group"]
      when (null content) (fail "Empty content")
      pure $ AlgoliaRecord
        { title
        , logo
        , permalink = site <> "#" <> sectionId
        , section
        , content
        }
