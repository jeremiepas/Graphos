-- | GraphML export (for Gephi, yEd)
module Graphos.Infrastructure.Export.GraphML
  ( exportGraphML
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types ()
import Graphos.Domain.Graph (Graph)

-- | Export graph as GraphML
exportGraphML :: Graph -> FilePath -> IO ()
exportGraphML g path = do
  let xml = generateGraphML g
  writeFile path (T.unpack xml)

generateGraphML :: Graph -> Text
generateGraphML _g =
  T.unlines
    [ "<?xml version='1.0' encoding='UTF-8'?>"
    , "<graphml xmlns='http://graphml.graphstruct.org/xmlns'>"
    , "  <graph id='G' edgedefault='undirected'>"
    , "    <!-- nodes and edges will be injected -->"
    , "  </graph>"
    , "</graphml>"
    ]