{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Arch
  (PackagesStats (..)
  ,PackageStat
  ,ParsedDocument
  ,getListOfPackages
  ,parseDoc
  ,getPackagesStats
  ,getPackages
  ,searchPackageStats)
where

import Lib
import Text.XML.Cursor
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as Lazy
import Text.XML (Document)
import Data.List (find)
import Data.Char (isDigit)

type PackageStat = (Text, Int)

type DocumentParse = Document -> ParsedDocument
type ParsedDocument = CursorParseEither String [Cursor]

data PackagesStats = PackagesStats [PackageStat] deriving (Generic, Show)

instance ToJSON PackagesStats
instance FromJSON PackagesStats

getPackages :: PackagesStats -> [PackageStat]
getPackages (PackagesStats s) = s

getPackagesStats :: String -> IO (Either String PackagesStats)
getPackagesStats x = do
  fc <- Lazy.readFile x
  return $ eitherDecode fc

searchPackageStats :: PackagesStats -> Text -> Maybe PackageStat
searchPackageStats packageStats package = find ((== package) . fst) $ getPackages packageStats

getListOfPackages :: (Cursor, Cursor) -> CursorParseEither ([Maybe Text]) PackageStat
getListOfPackages (cursor, cursorb) = do
  let packageName = case (getContent . node $ cursor) of
        ("") -> Nothing
        v -> Just v
  let download = case (getContent . node $ cursorb) of
        ("") -> Nothing
        v -> Just v
  let required = [packageName, download] in
    case sequence required of
      Just (pname:pperc:[]) -> Right (pname, parsePkgValue pperc)
      _ -> Left ([cursor], required)
  where
    parsePkgValue :: Text -> Int
    parsePkgValue = read . filter isDigit . head . words . unpack

parseDoc :: DocumentParse
parseDoc = (\doc-> Right [fromDocument doc]
    >>= extract "ParseArchDoc E1" ($/ element "body")
    >>= extract "ParseArchDoc E2" ($/ element "div")
    >>= extract "ParseArchDoc E3" (attributeIs "id" "content")
    >>= extract "ParseArchDoc E5" ($/ element "div")
    >>= extract "ParseArchDoc E6" ($/ element "table")
    -- >>= extract "ParseArchDoc E9" ($/ element "tbody")
    >>= extract "ParseArchDoc E10" ($/ element "tr")
    >>= extract "ParseArchDoc E11b" ($/ element "td"))
