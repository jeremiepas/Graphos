-- | Graph output directory resolution.
-- Leaf module: depends on no other Graphos modules, so it can be imported
-- freely by Domain, Infrastructure, and the CLI without module cycles.
--
-- The canonical default lives here once; every consumer resolves the
-- effective output directory through 'resolveOutputDir' (or Core's
-- 'effectiveOutputDir' when it has a whole config) instead of hardcoding
-- the default name.
module Graphos.Domain.Config.Output
  ( defaultOutputDirName
  , resolveOutputDir
  ) where

-- | The built-in output directory name used when neither graphos.yaml
-- (@output:@) nor the CLI (@-o@) names one.
defaultOutputDirName :: FilePath
defaultOutputDirName = "graphos-out"

-- | Effective output directory resolution (multi-source-graphs 2.2):
--
-- * an explicit CLI @-o@ always wins,
-- * otherwise the graphos.yaml @output:@ key when set,
-- * otherwise 'defaultOutputDirName'.
--
-- The CLI value is the parser's default whenever no @-o@ was passed, so the
-- presence bit — not string comparison — decides.
resolveOutputDir :: Bool           -- ^ was an explicit @-o@ flag passed?
                 -> FilePath       -- ^ the CLI @-o@ value
                 -> Maybe FilePath -- ^ the graphos.yaml @output:@ value
                 -> FilePath
resolveOutputDir cliGiven cliOut mCfgOut = maybe defaultOutputDirName id decided
  where
    decided :: Maybe FilePath
    decided
      | cliGiven  = Just cliOut
      | otherwise = mCfgOut