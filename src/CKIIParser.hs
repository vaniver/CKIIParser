{-# LANGUAGE OverloadedStrings #-}
module CKIIParser where

import           Data.Char                      ( isSpace )
import           Data.Map                       ( Map(..)
                                                , fromList
                                                )
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
data SaveFileValue = TextValue Text | ListValue [Integer] | MapValue SaveFileMap deriving Show


-- Save file parser
-- -------------------------------------------------------------------------------------------------

parseSaveFile :: Text -> IO ()
parseSaveFile = parseTest pSaveFileMap

pSaveFileMap :: Parser SaveFileMap
pSaveFileMap = fromList <$> between (symbol "CK2txt") eof (many pKeyValue)


-- Key value pair parser
-- -------------------------------------------------------------------------------------------------

pKeyValue :: Parser (Key, SaveFileValue)
pKeyValue = (,) . Key <$> pKey <*> pValue

pKey :: Parser Text
pKey = pack <$> manyTill L.charLiteral (symbol "=")

pValue :: Parser SaveFileValue
pValue = choice [try pListValue, pMapValue, pStringValue] <* sc


-- SaveFileValue parsers
-- -------------------------------------------------------------------------------------------------

pListValue :: Parser SaveFileValue
pListValue = betweenBraces $ ListValue <$> many listInteger
  where integer     = lexeme L.decimal
        listInteger = L.signed sc integer

pMapValue :: Parser SaveFileValue
pMapValue = MapValue . fromList <$> (symbol "{" *> manyTill pKeyValue (symbol "}"))
-- TODO: Figure out why the below doesn't work
-- pSaveMap = MapValue . fromList <$> betweenBraces (many pKeyValue)

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
