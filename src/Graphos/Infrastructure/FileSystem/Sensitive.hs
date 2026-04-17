-- | Sensitive file detection - skip files containing secrets/credentials
module Graphos.Infrastructure.FileSystem.Sensitive
  ( isSensitive
  , sensitivePatterns
  ) where

import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import System.FilePath (takeFileName, takeDirectory)

-- | Check if a file path matches sensitive/secret patterns
isSensitive :: FilePath -> Bool
isSensitive path =
  let name = takeFileName path
      dir  = takeDirectory path
  in any (`matchSensitive` name) sensitivePatterns
     || any (`matchSensitive` dir) directoryPatterns

-- | Patterns for sensitive files (basenames or suffixes)
sensitivePatterns :: [String]
sensitivePatterns =
  [ ".env"
  , ".env.local"
  , ".env.production"
  , ".env.staging"
  , ".pem"
  , ".key"
  , ".p12"
  , ".pfx"
  , ".netrc"
  , ".npmrc"
  , ".pypirc"
  , "id_rsa"
  , "id_ed25519"
  , "id_ecdsa"
  , "id_dsa"
  , "credentials"
  , "secrets"
  , "secret_key"
  , "service-account"
  , "service_account"
  , ".aws/credentials"
  , ".gnupg"
  , "ssh_config"
  , ".ssh/config"
  , ".docker/config.json"
  , ".kube/config"
  ]

-- | Patterns for sensitive directories
directoryPatterns :: [String]
directoryPatterns =
  [ ".gnupg"
  , ".ssh"
  ]

-- | Match a pattern against a filename or path
matchSensitive :: String -> FilePath -> Bool
matchSensitive pattern target
  | headOrDefault pattern == '.' && '.' `elem` tail pattern =
      pattern `isSuffixOf` target || pattern `isInfixOf` target
  | otherwise =
      target == pattern || pattern `isInfixOf` target

headOrDefault :: String -> Char
headOrDefault [] = ' '
headOrDefault (x:_) = x