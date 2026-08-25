-- | Parser for the supported read-only openCypher/GQL subset.
--
-- Grammar (frozen in openspec change opencypher-gql-query, design.md):
--
--   query        := MATCH patternList [WHERE predicate] RETURN returnClause
--   patternList  := patternElem (',' patternElem)*
--   patternElem  := nodePat (relPat nodePat)*
--   nodePat      := '(' varName [':' labelName]* ['{' props '}'] ')'
--   relPat       := '-' '[' [varName] [':' types] [hops] [props] ']' dir '(' nodePatInner ')'
--   predicate    := orExpr
--   orExpr       := andExpr ('OR' andExpr)*
--   andExpr      := notExpr ('AND' notExpr)*
--   notExpr      := 'NOT' notExpr | primary
--   primary      := '(' predicate ')' | comparison
--   comparison   := propRef op expr
--                  | propRef 'IN' '(' exprList ')'
--                  | propRef 'IS' ['NOT'] 'NULL'
--                  | propRef 'STARTS' 'WITH' expr
--                  | propRef 'CONTAINS' expr
--                  | propRef '=~' expr
--   returnClause := [DISTINCT] returnItems [ORDER BY orderItems] [SKIP expr] [LIMIT expr]
--
-- Keywords are case-insensitive. Anything outside the subset is a parse
-- error with position, not a silent fallback.
module Graphos.Domain.Query.Cypher.Parser
  ( parseQuery
  ) where

import Control.Applicative (optional, (<|>))
import Control.Monad (unless, void, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Void (Void)

import Text.Megaparsec
  ( Parsec
  , MonadParsec (..)
  , parse
  , satisfy
  , oneOf
  , (<?>)
  , sepBy1
  , many
  , some
  , sepBy
  )
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Error (errorBundlePretty)

import Graphos.Domain.Query.Cypher.AST

-- ───────────────────────────────────────────────
-- Entry point
-- ───────────────────────────────────────────────

type Parser = Parsec Void Text

-- | Parse a query. Returns the query or a human-readable error message
-- (position + unexpected/expected tokens).
parseQuery :: Text -> Either Text CypherQuery
parseQuery input =
  case parse (query <?> "query") "query" input of
    Left bundle  -> Left (T.pack (errorBundlePretty bundle))
    Right q      -> Right q

-- ───────────────────────────────────────────────
-- Lexing helpers
-- ───────────────────────────────────────────────

-- | Skip whitespace (zero or more).
ws :: Parser ()
ws = space

isIdentChar :: Char -> Bool
isIdentChar c = c == '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
            || c >= '0' && c <= '9'

-- | A case-insensitive keyword. The input must be exactly the keyword
-- (a maximal identifier run), so `MATCHX` does not match `MATCH`.
keyword :: Text -> Parser ()
keyword k = try $ do
  r <- takeWhile1P (Just (T.unpack k)) isIdentChar
  unless (T.toLower r == k) (fail ("unexpected keyword: " ++ T.unpack r))

-- | An identifier: [a-zA-Z_][a-zA-Z0-9_]*.
ident :: Parser Text
ident = do
  c <- oneOf ("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" :: [Char])
  r <- many (satisfy isIdentChar)
  pure (T.pack (c : r))

-- | A variable name: an identifier that is not a reserved word.
varName :: Parser Text
varName = do
  r <- takeWhile1P (Just "variable") isIdentChar
  when (T.toLower r `elem` reservedWords) (fail ("reserved word: " ++ T.unpack (T.toLower r)))
  pure r

reservedWords :: [Text]
reservedWords =
  [ "and", "or", "not", "in", "is", "null", "true", "false"
  , "starts", "with", "contains", "as", "order", "by", "asc", "desc"
  , "distinct", "skip", "limit", "count", "where", "return", "match"
  ]

-- | A label / type name (any identifier).
labelName :: Parser Text
labelName = takeWhile1P (Just "label") isIdentChar

-- | A single-quoted string literal with '' escape.
stringLiteral :: Parser Text
stringLiteral = do
  void (char '\'')
  chunks <- many (stringChunk <|> escapedQuote)
  void (char '\'')
  pure (T.concat chunks)
  where
    escapedQuote :: Parser Text
    escapedQuote = try $ do
      void (char '\'')
      void (char '\'')
      pure (T.singleton '\'')
    stringChunk :: Parser Text
    stringChunk = takeWhile1P (Just "string") (/= '\'')

-- | A number literal (integer or floating point, optional sign).
number :: Parser Double
number = do
  neg <- optional (char '-')
  ds <- some (oneOf ("0123456789" :: [Char]))
  frac <- optional (char '.' *> some (oneOf ("0123456789" :: [Char])))
  let numStr = case frac of
        Just f  -> ds ++ "." ++ f
        Nothing -> ds
      val = read numStr :: Double
  pure (if neg == Just '-' then -val else val)

-- | A non-negative integer.
natural :: Parser Int
natural = do
  ds <- some (oneOf ("0123456789" :: [Char]))
  pure (read ds)

-- ───────────────────────────────────────────────
-- Query
-- ───────────────────────────────────────────────

query :: Parser CypherQuery
query = do
  ws
  keyword "match"
  ws
  pats <- patternList
  ws
  wh <- optional $ do
    try (keyword "where")
    ws
    p <- predicate
    ws
    pure p
  keyword "return"
  ws
  rc <- returnClause
  ws
  eof
  pure (CypherQuery pats wh rc)

-- ───────────────────────────────────────────────
-- Patterns
-- ───────────────────────────────────────────────

patternList :: Parser [PatternElem]
patternList = do
  xs <- sepBy1 patternElem (try (ws *> char ',' <* ws))
  pure (concat xs)

-- | A pattern element: a node pattern optionally followed by a chain of
-- relationship patterns, each introducing a new node.
--
--   (a)-[:R]->(b)-[:S]->(c)
--     => [NodePatE a, RelPatE (a-b), NodePatE b, RelPatE (b-c), NodePatE c]
patternElem :: Parser [PatternElem]
patternElem = do
  np <- nodePat
  chain np

chain :: NodePat -> Parser [PatternElem]
chain cur = do
  r <- try (relPatChain cur)
  case r of
    Just (rel, nextNode) -> do
      rest <- chain nextNode
      pure (NodePatE cur : RelPatE rel : rest)
    Nothing -> pure [NodePatE cur]

nodePat :: Parser NodePat
nodePat = do
  void (char '(')
  ws
  np <- nodePatInner
  ws
  void (char ')')
  pure np

nodePatInner :: Parser NodePat
nodePatInner = do
  v <- varName
  ws
  labels <- many (char ':' *> ws *> labelName <* ws)
  props <- optional $ do
    void (char '{')
    ws
    ps <- propsMap
    ws
    void (char '}')
    pure ps
  pure (NodePat v labels (fromMaybe Map.empty props))

-- | Parse a relationship pattern following the source node `cur`.
-- Consumes `- [ ... ] dir ( target )` and returns the relationship plus
-- the target node pattern.
relPatChain :: NodePat -> Parser (Maybe (RelPat, NodePat))
relPatChain cur = optional $ do
  leftArrow <- try (char '<' *> pure True) <|> pure False
  void (char '-')
  ws
  (rVar, rTypes, rHops, rProps) <- relInner
  void (char '-')
  rightArrow <- try (char '>' *> pure True) <|> pure False
  ws
  void (char '(')
  ws
  tp <- nodePatInner
  ws
  void (char ')')
  let dir = if leftArrow then DirLeft
            else if rightArrow then DirRight
            else DirUndirected
  let rel = RelPat
        { rpFrom  = npVar cur
        , rpTo    = npVar tp
        , rpVar   = rVar
        , rpTypes = rTypes
        , rpDir   = dir
        , rpHops  = rHops
        , rpProps = rProps
        }
  pure (rel, tp)

-- | The bracketed relationship body: [var [:types] [*hops] {props}].
relInner :: Parser (Maybe Text, [Text], HopRange, Map Text Expr)
relInner = do
  void (char '[')
  ws
  v <- optional (varName <* ws)
  t <- optional $ do
    void (char ':')
    ws
    ts <- some (labelName <* ws)
    pure ts
  h <- optional $ do
    void (char '*')
    ws
    lo <- optional (natural <* ws)
    hi <- optional $ do
      void (char '.')
      void (char '.')
      ws
      n <- natural
      ws
      pure n
    let minH = fromMaybe 1 lo
    let maxH = fromMaybe minH hi
    pure (HopRange minH maxH)
  p <- optional $ do
    void (char '{')
    ws
    ps <- propsMap
    ws
    void (char '}')
    pure ps
  ws
  void (char ']')
  pure (v, fromMaybe [] t, fromMaybe (HopRange 1 1) h, fromMaybe Map.empty p)



propsMap :: Parser (Map Text Expr)
propsMap = do
  pairs <- sepBy propEntry (ws *> char ',' <* ws)
  pure (Map.fromList pairs)

propEntry :: Parser (Text, Expr)
propEntry = do
  k <- ident
  ws
  void (char ':')
  ws
  v <- expr
  ws
  pure (k, v)

-- ───────────────────────────────────────────────
-- Predicates
-- ───────────────────────────────────────────────

predicate :: Parser Predicate
predicate = orExpr

orExpr :: Parser Predicate
orExpr = do
  a <- andExpr
  rest <- many $ do
    keyword "or"
    ws
    b <- andExpr
    pure b
  pure (foldl POr a rest)

andExpr :: Parser Predicate
andExpr = do
  a <- notExpr
  rest <- many $ do
    keyword "and"
    ws
    b <- notExpr
    pure b
  pure (foldl PAnd a rest)

notExpr :: Parser Predicate
notExpr =
    do
      keyword "not"
      ws
      p <- notExpr
      pure (PNot p)
  <|> primary

primary :: Parser Predicate
primary =
    do
      void (char '(')
      ws
      p <- predicate
      ws
      void (char ')')
      pure p
  <|> comparison

-- | Local dispatch: the six CompareOps plus the special operators.
data Op
  = OpCmp CompareOp
  | OpRegex
  | OpIn
  | OpIs
  | OpStartsWith
  | OpContains

opParser :: Parser Op
opParser =
    try (char '=' *> char '~' *> pure OpRegex)
 <|> try (char '<' *> char '>' *> pure (OpCmp OpNeq))
 <|> try (char '<' *> char '=' *> pure (OpCmp OpLe))
 <|> try (char '>' *> char '=' *> pure (OpCmp OpGe))
 <|> (char '=' *> pure (OpCmp OpEq))
 <|> (char '<' *> pure (OpCmp OpLt))
 <|> (char '>' *> pure (OpCmp OpGt))
 <|> (keyword "in" *> ws *> pure OpIn)
 <|> (keyword "is" *> ws *> pure OpIs)
 <|> (keyword "starts" *> ws *> keyword "with" *> ws *> pure OpStartsWith)
 <|> (keyword "contains" *> ws *> pure OpContains)

-- | A single comparison / membership / string / regex predicate.
comparison :: Parser Predicate
comparison = do
  ref <- propRef
  ws
  op <- opParser
  case op of
    OpCmp c -> do
      ws
      e <- expr
      ws
      pure (PCompare ref c e)
    OpRegex -> do
      ws
      e <- expr
      ws
      pure (PRegex ref e)
    OpIn -> do
      ws
      vals <- char '(' *> ws *> sepBy1 expr (ws *> char ',' <* ws) <* ws <* char ')'
      ws
      pure (PIn ref vals)
    OpIs -> do
      ws
      neg <- (try $ do
        keyword "not"
        ws
        pure True
        ) <|> pure False
      keyword "null"
      ws
      pure (PIsNull ref (not neg))
    OpStartsWith -> do
      ws
      e <- expr
      ws
      pure (PStartsWith ref e)
    OpContains -> do
      ws
      e <- expr
      ws
      pure (PContains ref e)

-- ───────────────────────────────────────────────
-- Expressions
-- ───────────────────────────────────────────────

expr :: Parser Expr
expr =
  countExpr
    <|> literal
    <|> propRefExpr

countExpr :: Parser Expr
countExpr = do
  keyword "count"
  ws
  void (char '(')
  ws
  arg <- try (char '*' *> pure Nothing)
       <|> (Just <$> varName)
  ws
  void (char ')')
  pure (ECount arg)

literal :: Parser Expr
literal =
    do
      keyword "true"
      pure (EBool True)
  <|> do
      keyword "false"
      pure (EBool False)
  <|> do
      keyword "null"
      pure ENull
  <|> (EStr <$> stringLiteral)
  <|> (ENum <$> number)

propRefExpr :: Parser Expr
propRefExpr = do
  r <- propRef
  case r of
    VarRef v    -> pure (EVar v)
    PropRef v p -> pure (EProp v p)

propRef :: Parser PropRef
propRef = do
  v <- varName
  ws
  p <- optional (char '.' *> ws *> ident)
  case p of
    Just p' -> pure (PropRef v p')
    Nothing -> pure (VarRef v)

-- ───────────────────────────────────────────────
-- Return
-- ───────────────────────────────────────────────

distinctParser :: Parser Bool
distinctParser =
  let distinctYes = do
        keyword "distinct"
        ws
        pure True
  in try distinctYes <|> pure False

returnClause :: Parser ReturnClause
returnClause = do
  distinct <- distinctParser
  items <- returnItems
  orderBy <- optional $ do
    keyword "order"
    ws
    keyword "by"
    ws
    os <- orderItems
    ws
    pure os
  skip <- optional $ do
    keyword "skip"
    ws
    n <- natural
    ws
    pure n
  limit <- optional $ do
    keyword "limit"
    ws
    n <- natural
    ws
    pure n
  pure (ReturnClause distinct items (fromMaybe [] orderBy) skip limit)

returnItems :: Parser [ReturnItem]
returnItems = sepBy1 returnItem (ws *> char ',' <* ws)

returnItem :: Parser ReturnItem
returnItem =
    do
      keyword "count"
      ws
      void (char '(')
      ws
      arg <- try (char '*' *> pure Nothing)
           <|> (Just <$> varName)
      ws
      void (char ')')
      ws
      alias <- optional $ do
        keyword "as"
        ws
        n <- ident
        ws
        pure n
      pure (RICount arg alias)
  <|> do
      e <- expr
      ws
      alias <- optional $ do
        keyword "as"
        ws
        n <- ident
        ws
        pure n
      ws
      pure (RIExpr e alias)

orderItems :: Parser [OrderItem]
orderItems = sepBy1 orderItem (ws *> char ',' <* ws)

orderItem :: Parser OrderItem
orderItem = do
  r <- propRef
  ws
  desc <- (try $ do
    keyword "desc"
    ws
    pure True
    ) <|> (do
    keyword "asc"
    ws
    pure False
    ) <|> pure False
  pure (OrderItem r desc)
