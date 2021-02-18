{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Kip.Parser where

import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Control.Monad.IO.Class

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Expr as P

import Language.Foma
import Kip.AST

data ParserState =
  MkParserState
    { fsm :: FSM
    , parserCtx :: [Identifier]
    }

-- We need [IO] access in here because we need morphological parsing.
type Outer = IO
type KipParser = ParsecT String ParserState Outer

period :: KipParser ()
period = spaces >> string "." >> spaces

lexeme :: KipParser a -> KipParser a
lexeme p = spaces *> p <* spaces

parens :: KipParser a -> KipParser a
parens p = char '(' *> p <* char ')'

name :: KipParser String
name = (:) <$> letter <*> many (digit <|> letter)

word :: KipParser String
word = many1 letter

multiword :: KipParser String
multiword = unwords <$> many1 (lexeme word)


-- Start copied from https://stackoverflow.com/a/24106749/2016295, CC BY-SA 3.0
escape :: KipParser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: KipParser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: KipParser String
character = fmap return nonEscape <|> escape

parseQuotedString :: KipParser String
parseQuotedString = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings
-- End copied from https://stackoverflow.com/a/24106749/2016295, CC BY-SA 3.0

identifier :: KipParser Identifier
identifier = do
  ws <- sepBy1 word (char '-') <?>
        "Tek kelimeli veya tire ile ayrılmış çok kelimeli bir isim kullanmanız gerek."
  return (init ws, last ws)

-- inCtx :: String -> KipParser Bool
-- inCtx x = do
--   MkParserState{..} <- getState
--   return $ x `elem` parserCtx

getPossibleCase :: String -> Maybe (String, Case)
getPossibleCase s 
  | (root : _ : _) <- splitOn "<acc>" s = Just (root, Acc)
  | (root : _ : _) <- splitOn "<dat>" s = Just (root, Dat)
  | (root : _ : _) <- splitOn "<loc>" s = Just (root, Loc)
  | (root : _ : _) <- splitOn "<abl>" s = Just (root, Abl)
  | (root : _ : _) <- splitOn "<gen>" s = Just (root, Gen)
  | (root : _ : _) <- splitOn "<ins>" s = Just (root, Ins)
  | (root : _ : _) <- splitOn "<ise>" s = Just (root, Cond)
  | otherwise = Just (s, Nom) -- FIXME, this is very rudimentary

-- | Takes the fully declined word, returns the possible case
-- and the string consisting the original word stripped of this case,
-- based on the values in the context.
estimateCase :: Identifier -> KipParser (Identifier, Case)
estimateCase (ss, s) = do
  MkParserState{..} <- getState
  morphAnalyses <- liftIO (ups fsm s)
  let xs = nub $ [ ((ws, root), cas) | (ws, w) <- parserCtx
                               , y <- morphAnalyses
                               , let Just (root, cas) = getPossibleCase y
                               , w `isPrefixOf` y
                               , ss == ws ]
  case xs of
    [] -> fail "Buraya uyan yalın halde bir isim bulunamadı."
    _:_:_ -> fail $ "Belirsizlik: " ++ intercalate ", " (map show xs)
    [p] -> return p

casedIdentifier :: KipParser (Identifier, Case)
casedIdentifier = identifier >>= estimateCase

parseExp :: KipParser (Exp Case)
parseExp = 
    try var <|> parens parseExp
    -- <|> str 
  where
    -- var = Var Nom <$> identifier 
    var = casedIdentifier >>= \(s,c) -> return (Var c s)
    -- str = StrLit <$> parseQuotedString

parseStmt :: KipParser Stmt
parseStmt = ty <|> try def <|> expFirst
  where
    ctor = try ((, []) <$> identifier) 
      -- <|> parens (do 
      -- n <- identifier 
      -- -- TODO many parseTy
      -- return (n, [("arg", (TyString, Nom))]))
    ya = lexeme (string "ya")
    ty = do
      lexeme (string "Bir")
      n <- identifier
      ctors <- try (lexeme (string "var olamaz") *> return [])
           <|> ((ya *> sepBy1 ctor (try ya)) <* lexeme (string "olabilir"))
      period
      return (NewType n ctors)
    def = do
      i <- identifier
      lexeme (string "diyelim")
      period
      modifyState (\ps -> ps {parserCtx = i : (parserCtx ps)})
      return (Defn i (TyString Nom) (StrLit Nom "foo"))
    expFirst = do
      e <- parseExp
      (try (lexeme (string "yazdır") *> return (Print e)) <|> 
        return (ExpStmt e)) <* period

-- | Too simple but it should do the job for now.
removeComments :: String -> String
removeComments s = go 0 s
  where
    go _ []             = []
    go n ('(':'*':rest) = go (n + 1) rest
    go n ('*':')':rest) = go (n - 1) rest
    go n (c:cs) | n < 0 = error "Closing comment without opening"
                | n == 0 = c : go n cs
                | otherwise = go n cs

parseFromRepl :: ParserState -> String -> Outer (Either ParseError (Stmt, ParserState))
parseFromRepl st input = runParserT p st "Kip" (removeComments input)
  where p = (,) <$> parseStmt <*> getState
