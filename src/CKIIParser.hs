{-# LANGUAGE OverloadedStrings #-}
module CKIIParser where

import           Data.Char                      ( isSpace )
import           Data.Map                       ( Map(..)
                                                , fromList
                                                , size
                                                )
import           Data.Scientific                ( Scientific(..) )
import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.Void                      ( Void(..) )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


-- Types
-- -------------------------------------------------------------------------------------------------

type Parser = Parsec Void Text

newtype Key = Key Text deriving (Show, Eq, Ord)
type SaveFileMap = Map Key SaveFileValue
data SaveFileList = IntListValue [Integer] | SciListValue [Scientific] | SaveFileMapList [SaveFileValue] deriving Show
data SaveFileValue = TextValue Text | MapValue SaveFileMap | SaveFileValueList SaveFileList deriving Show


-- Save file parser
-- -------------------------------------------------------------------------------------------------

parseSaveFile :: Text -> IO ()
parseSaveFile input = case (parse pSaveFileMap "whatever" input) of
  Left x -> putStrLn (show x)
  Right y ->  putStrLn (show (size y))

parseTestSaveFile :: Text -> IO ()
parseTestSaveFile = parseTest pSaveFileMap

pSaveFileMap :: Parser SaveFileMap
pSaveFileMap = fromList <$> (symbol "CK2txt" *> manyTill pKeyValue (symbol "}")) <* eof


-- Key value pair parser
-- -------------------------------------------------------------------------------------------------

pKeyValue :: Parser (Key, SaveFileValue)
pKeyValue = (,) . Key <$> pKey <*> pValue

pKey :: Parser Text
pKey = pack <$> manyTill L.charLiteral (symbol "=")

pValue :: Parser SaveFileValue
pValue = choice [try pIntListValue, try pSciListValue, try pSaveFileMapList, pMapValue, pStringValue] <* sc


-- SaveFileValue parsers
-- -------------------------------------------------------------------------------------------------

pIntListValue :: Parser SaveFileValue
pIntListValue = betweenBraces $ SaveFileValueList . IntListValue <$> many listInteger
  where integer     = lexeme L.decimal
        listInteger = L.signed sc integer

pSciListValue :: Parser SaveFileValue
pSciListValue = betweenBraces $ SaveFileValueList . SciListValue <$> many listScientific
  where scientific     = lexeme L.scientific
        listScientific = L.signed sc scientific

pSaveFileMapList :: Parser SaveFileValue
pSaveFileMapList = betweenBraces $ SaveFileValueList . SaveFileMapList <$> many pMapValue

pMapValue :: Parser SaveFileValue
pMapValue = MapValue . fromList <$> (symbol "{" *> manyTill pKeyValue (symbol "}"))

pStringValue :: Parser SaveFileValue
pStringValue = choice
  [ TextValue . pack <$> (char '"' *> manyTill L.charLiteral (char '"'))
  , TextValue <$> takeWhileP Nothing (not . isSpace)
  ]

-- Lexer helpers
-- -------------------------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

betweenBraces :: Parser a -> Parser a
betweenBraces = between (symbol "{") (symbol "}")