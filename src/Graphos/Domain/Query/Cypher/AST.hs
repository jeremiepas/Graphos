-- | AST for the supported read-only openCypher/GQL subset.
--
-- The subset is frozen in the spec (openspec change opencypher-gql-query):
--   * MATCH with node and relationship patterns (incl. variable-length)
--   * WHERE with comparison / membership / string / regex predicates
--   * RETURN projection with DISTINCT, ORDER BY, SKIP, LIMIT, count
--
-- Anything outside this subset is a parse error, not a silent fallback.
module Graphos.Domain.Query.Cypher.AST
  ( -- * Query
    CypherQuery(..)

    -- * Patterns
  , PatternElem(..)
  , NodePat(..)
  , RelPat(..)
  , RelDir(..)
  , HopRange(..)

    -- * Predicates
  , Predicate(..)
  , CompareOp(..)
  , PropRef(..)

    -- * Return
  , ReturnClause(..)
  , ReturnItem(..)
  , OrderItem(..)

    -- * Expressions
  , Expr(..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)

-- ───────────────────────────────────────────────
-- Query
-- ───────────────────────────────────────────────

-- | A parsed query: one MATCH clause, an optional WHERE, and a RETURN.
data CypherQuery = CypherQuery
  { cqPatterns :: [PatternElem]
    -- ^ The comma-separated pattern elements of the MATCH clause.
  , cqWhere    :: Maybe Predicate
  , cqReturn   :: ReturnClause
  } deriving (Eq, Show)

-- ───────────────────────────────────────────────
-- Patterns
-- ───────────────────────────────────────────────

-- | A single pattern element in the MATCH clause.
data PatternElem
  = NodePatE NodePat
  | RelPatE RelPat
  deriving (Eq, Show)

-- | A node pattern: `(a:Function:Kind {prop: value})`.
data NodePat = NodePat
  { npVar    :: Text
    -- ^ The variable bound by this pattern (required in the subset).
  , npLabels :: [Text]
    -- ^ Zero or more labels; the node must carry all of them.
  , npProps  :: Map Text Expr
    -- ^ Property constraints; the node's property must equal the value.
  } deriving (Eq, Show)

-- | A relationship pattern: `(a)-[r:REL*1..3]->(b)`.
data RelPat = RelPat
  { rpFrom   :: Text
    -- ^ Source node variable.
  , rpTo     :: Text
    -- ^ Target node variable.
  , rpVar    :: Maybe Text
    -- ^ Optional relationship variable.
  , rpTypes  :: [Text]
    -- ^ Relationship types; empty means any type.
  , rpDir    :: RelDir
  , rpHops   :: HopRange
  , rpProps  :: Map Text Expr
    -- ^ Property constraints on the relationship.
  } deriving (Eq, Show)

-- | Relationship direction.
data RelDir
  = DirRight
    -- ^ `(a)-[...]->(b)`: follows edge direction.
  | DirLeft
    -- ^ `(a)<-[...]- (b)`: against edge direction.
  | DirUndirected
    -- ^ `(a)-[...]- (b)`: either direction.
  deriving (Eq, Show)

-- | Variable-length hop range. The default (no `*` in the query) is 1..1.
data HopRange = HopRange
  { hrMin :: Int
  , hrMax :: Int
  } deriving (Eq, Show)

-- ───────────────────────────────────────────────
-- Predicates
-- ───────────────────────────────────────────────

-- | A WHERE predicate.
data Predicate
  = PAnd Predicate Predicate
  | POr  Predicate Predicate
  | PNot Predicate
  | PIsNull PropRef Bool
    -- ^ `ref IS NULL` / `ref IS NOT NULL`
  | PCompare PropRef CompareOp Expr
  | PIn PropRef [Expr]
  | PStartsWith PropRef Expr
  | PContains PropRef Expr
  | PRegex PropRef Expr
  deriving (Eq, Show)

-- | Comparison operator.
data CompareOp
  = OpEq
  | OpNeq
  | OpLt
  | OpLe
  | OpGt
  | OpGe
  deriving (Eq, Show)

-- | A reference to a bound variable or one of its properties.
data PropRef
  = VarRef Text
  | PropRef Text Text
    -- ^ `var.prop`
  deriving (Eq, Show)

-- ───────────────────────────────────────────────
-- Return
-- ───────────────────────────────────────────────

data ReturnClause = ReturnClause
  { rcDistinct :: Bool
  , rcItems    :: [ReturnItem]
  , rcOrderBy  :: [OrderItem]
  , rcSkip     :: Maybe Int
  , rcLimit    :: Maybe Int
  } deriving (Eq, Show)

-- | A single RETURN item.
data ReturnItem
  = RIExpr Expr (Maybe Text)
    -- ^ `expr [AS alias]`
  | RICount (Maybe Text) (Maybe Text)
    -- ^ `count(var | *) [AS alias]`; first Nothing = `*`.
  deriving (Eq, Show)

-- | An ORDER BY item: a variable or variable.property, optionally DESC.
data OrderItem = OrderItem
  { oiRef  :: PropRef
  , oiDesc :: Bool
  } deriving (Eq, Show)

-- ───────────────────────────────────────────────
-- Expressions
-- ───────────────────────────────────────────────

-- | A scalar expression (literals, variable/property references, count).
data Expr
  = EVar Text
  | EProp Text Text
    -- ^ `var.prop`
  | EStr Text
  | ENum Double
  | EBool Bool
  | ENull
  | ECount (Maybe Text)
    -- ^ `count(var | *)`; Nothing = `*`.
  deriving (Eq, Show)
