{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$+-))
import Network.HTTP.Conduit (tlsManagerSettings, http, newManager, parseUrlThrow, responseBody)
import qualified Text.HTML.DOM as THX (readFile, sinkDoc)
import Text.XML as TX (Document)
import Data.Text as DT (Text, unpack)
import qualified Data.Text as DT (strip, words, unwords)
import Data.String.Utils (strip, rstrip)
import Text.XML
import Text.XML.Cursor (node, Cursor)
import qualified Text.XML.Cursor.Generic as XMLG

type CursorParseLeft a = ([Cursor], a)
type CursorParseEither failType succListType = Either (CursorParseLeft failType) succListType

getDocumentFile :: FilePath -> IO Document
getDocumentFile path = do
  THX.readFile path

makeRequest :: String -> IO Document
makeRequest url = do
  request <- parseUrlThrow url
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager
    let body = responseBody response
    body $$+- THX.sinkDoc

formatChildString :: String -> String
formatChildString string = rstrip . unlines . map prefixStringIndent $ lines string

prefixStringIndent :: String -> String
prefixStringIndent = (++ "    ")

filterString :: String -> String
filterString = strip . unwords . words

filterFloatString :: String -> String
filterFloatString = filterString . filter (/= '%')

filterText :: Text -> Text
filterText = DT.strip . DT.unwords . DT.words

cursorContentIs :: Text -> Cursor -> Bool
cursorContentIs text cursor = (getContent . node $ cursor) == text

contentIs :: Text -> Cursor -> [Cursor]
contentIs text cursor = case cursorContentIs text cursor of
  False -> []
  True -> [cursor]

getContent :: Node -> Text
getContent (NodeElement e) = mconcat . map getContent $ elementNodes e
getContent (NodeContent c) = c
getContent _ = "Ignored: Node Instruction / Node Comment"

extract :: t -> (a -> [b]) -> [a] -> Either ([a], t) [b]
extract es f x = case (x >>= f) of
  [] -> Left (x, es)
  y -> Right y

printCursor :: [XMLG.Cursor Node] -> String
printCursor cursor = concat ( map (printBasicNode . node) cursor)

printBasicNode :: Node -> String
printBasicNode nodePrint@(NodeElement element) = mainNode ++ "Chlidren: \n" ++ children where
  mainNode = (unlines [
      "Main node: "
      , printBasicNodeElements nodePrint
    ])
  children = concatMap unlines [
      map (formatChildString . printBasicNodeElements) (elementNodes element)
    ]
printBasicNode (NodeContent content) = (++ "Content:") . filterString $ unpack content
printBasicNode _ = "Ignored: Node Instruction / Node Comment"

printBasicNodeElements :: Node -> String
printBasicNodeElements (NodeElement element) = unlines
  [
    "element: " ++ (unpack . nameLocalName $ elementName element)
    , prefixStringIndent "attributes: " ++ (show $ elementAttributes element)
 ]

printBasicNodeElements (NodeContent content) = "element content: " ++ (filterString . unpack $ content)
printBasicNodeElements _ = "Ignored: Node Instruction / Node Comment"
