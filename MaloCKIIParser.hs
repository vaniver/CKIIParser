module MaloCKIIParser where

data SaveFileMap = Map Text SaveValue deriving Show
data SaveValue = Value Text | ListValue [Int] | SaveMap SaveFileMap deriving Show

parseSaveFile :: Text -> IO ()
parseSaveFile = parserTest pWholeSaveFileMap

pWholeSaveFileMap :: Parser SaveFileMap
pWholeSaveFileMap = do
  _   <- string "CK2txt"
  _   <- takeWhileP Nothing isSpace
  kvs <- manyTill pKeyValue eof
  return fromList kvs 

pKeyValue :: Parser (Text, SaveValue)
pKeyValue = do
  k <- pKey
  _ <- char '='
  _ <- takeWhileP Nothing isSpace
  v <- pValue
  
pKey :: Parser Text 
pKey = takeWhile1P Nothing (/= '=') 

pValue :: Parser SaveValue
pValue = do
  x <- choice
    [ listLiteral
    , pSaveMap 
    , stringLiteral 
    , stringValue ]
  return x

--{-# LANGUAGE OverloadedStrings #-}
--module CKIIParser where

-- import Control.Applicative
-- import Control.Monad
-- import Data.Text
-- import Data.Void
-- import Data.Char (isAlphaNum, isDigit, isSpace)
-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import qualified Data.Map as Map
-- import qualified Data.ByteString as B
-- import qualified Data.Text as T
-- import qualified Text.Megaparsec.Char.Lexer as L

-- import Debug.Trace

-- type Parser = Parsec Void Text

-- --f :: Text -> IO ()
-- --f = parseTest (string "CK2txt" :: Parser Text) 

-- -- newtype ProvID = ProvID Int
-- -- newtype ChrID = ChrID Int

-- -- data Date = Date
-- --   { year :: Int
-- --   , month :: Int
-- --   , day :: Int
-- --   }


-- -- sc :: Parser ()
-- -- sc = L.space space1 Text.Megaparsec.empty Text.Megaparsec.empty

-- -- lexeme :: Parser a -> Parser a
-- -- lexeme = L.lexeme sc

-- -- symbol :: Text -> Parser Text
-- -- symbol = L.symbol sc

-- charLiteral :: Parser Char
-- charLiteral = between (char '\'') (char '\'') L.charLiteral

-- stringLiteral :: Parser SaveValue
-- stringLiteral = do
--   x <- (char '\"' <?> "begin string literal") *> manyTill L.charLiteral (char '\"')
--   traceM $ "stringLiteral: " ++ x
--   return $ Value $ pack x

-- pNum :: Parser Int
-- pNum = do
--   negative <- optional . try $ do
--     neg <- char '-'
--     return neg
--   x <- takeWhileP Nothing isDigit
--   let y = read $ (unpack x) :: Int
--   case negative of 
--     Nothing   -> return y
--     otherwise -> return $ y * (-1)

-- pNumSpace :: Parser Int
-- pNumSpace = do
--   _ <- takeWhileP Nothing isSpace
--   x <- pNum
--   _ <- takeWhileP Nothing isSpace
--   return x

-- listLiteral :: Parser SaveValue
-- listLiteral = do
--     x <- (char '{' <?> "Begin list literal") *> manyTill pNumSpace (char '}' <?> "End list literal")
--     traceM $ "listLiteral: " ++ show x
--     return $ ListValue $ x

-- stringValue :: Parser SaveValue
-- stringValue = do 
--   x <- takeWhileP Nothing isAlphaNum
--   traceM $ "stringValue: " ++ show x
--   return $ Value x



-- pValue :: Parser SaveValue
-- pValue = do
--   x <- choice
--     [ stringLiteral 
--     , listLiteral
--     , pNestedSaveFileMap
--     , stringValue ]
--   traceM $ "pValue: " ++ show x
--   return x

-- pNestedSaveFileMap :: Parser SaveValue
-- pNestedSaveFileMap = do
--   (State _ s2 _) <- getParserState
--   traceM $ "Entered pNestedSaveFileMap " ++ show s2
--   _    <- takeWhileP Nothing isSpace
--   _    <- char '{' <?> "Begin nested map"
--   nsfm <- manyTill pSaveFileMap (char '}')
--   --_    <- takeWhileP Nothing isSpace
--   --_    <- char '}'
--   traceM $ "pNestedSaveFileMap: " ++ show nsfm
--   return $ SaveMap nsfm

-- pSaveFileMap :: Parser SaveFileMap
-- pSaveFileMap = do
--   traceM "Entered pSaveFileMap"
--   void (takeWhileP Nothing isSpace)
--   key <- pKey
--   void (char '=' <?> "KV pair")
--   value <- pValue
--   --void (takeWhileP Nothing isSpace)
--   traceM $ "pSaveFileMap: " ++ show (Map key value)
--   return $ Map key value
