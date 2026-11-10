-- | Pure hash helpers for Neo4j push keys.
--
-- Neo4j RANGE index keys cap at ~8 KB, and @External@ node ids embed whole
-- source snippets far beyond that limit, so an index or uniqueness constraint
-- on raw @id@ silently fails to populate and every lookup degrades to a label
-- scan (workflow 12). Every pushed node therefore carries @id_hash@, a
-- lowercase hex SHA-1 over the id's UTF-8 bytes — always 40 characters,
-- always indexable — and all @MERGE@/@MATCH@ lookups key on it instead.
--
-- The same hash must be computable identically server-side by the backfill
-- Cypher (@sha1(n.id)@), so this module pins down the exact parity contract:
-- lowercase hex, UTF-8 bytes, nothing else in the digest input.
module Graphos.Domain.CypherHash
  ( nodeIdHash
  , textSha1Hex
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16

import Graphos.Domain.Types.Node (NodeId)

-- | Lowercase hex SHA-1 of a 'NodeId''s UTF-8 bytes.
--
-- Always 40 lowercase hex characters; safe as an index key for any id.
nodeIdHash :: NodeId -> Text
nodeIdHash = textSha1Hex

-- | Lowercase hex SHA-1 over a text value's UTF-8 bytes.
--
-- Parity with the backfill Cypher @sha1()@ function: e.g.
-- @textSha1Hex "abc" = "a9993e364706816aba3e25717850c26c9cd0d89d"@.
textSha1Hex :: Text -> Text
textSha1Hex t =
  let digest = SHA1.hash (encodeUtf8 t)
      hex    = Base16.encode digest
  in T.toLower (decodeAsciiHex hex)
  where
    decodeAsciiHex = T.pack . map (toEnum . fromIntegral) . BS.unpack