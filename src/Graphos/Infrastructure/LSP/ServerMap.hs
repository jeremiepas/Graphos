-- | LSP language server registry — maps file extensions to LSP server commands.
module Graphos.Infrastructure.LSP.ServerMap
  ( languageServerCommands
  , findLSPServer
  , languageIdFromExt
  , takeExtension
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Directory (findExecutable)

-- | Map from file extension to (command, args) for LSP servers
languageServerCommands :: Map String (String, [String])
languageServerCommands = Map.fromList
  [ (".ts",  ("typescript-language-server", ["--stdio"]))
  , (".tsx", ("typescript-language-server", ["--stdio"]))
  , (".js",  ("typescript-language-server", ["--stdio"]))
  , (".jsx", ("typescript-language-server", ["--stdio"]))
  , (".py",  ("pyright-langserver", ["--stdio"]))
  , (".go",  ("gopls", []))
  , (".rs",  ("rust-analyzer", []))
  , (".c",   ("clangd", []))
  , (".cpp", ("clangd", []))
  , (".h",   ("clangd", []))
  , (".hpp", ("clangd", []))
  , (".java", ("jdtls", []))
  , (".cs",  ("omnisharp", []))
  , (".rb",  ("solargraph", ["--stdio"]))
  , (".hs",  ("haskell-language-server", ["--lsp"]))
  , (".lhs", ("haskell-language-server", ["--lsp"]))
  , (".php", ("phpactor", []))
  , (".swift", ("sourcekit-lsp", []))
  , (".kt",  ("kotlin-language-server", []))
  , (".kts", ("kotlin-language-server", []))
  , (".scala", ("metals", []))
  , (".lua", ("lua-language-server", []))
  , (".zig", ("zls", []))
  , (".ex",  ("elixir-ls", []))
  , (".exs", ("elixir-ls", []))
  , (".dart", ("dart", ["analyze", "--stdio"]))
  , (".vue", ("vue-language-server", []))
  , (".svelte", ("svelte-language-server", []))
  , (".nix", ("nixd", []))
  , (".json", ("vscode-json-language-server", ["--stdio"]))
  ]

-- | Find an LSP server for a file extension, checking if the command exists
findLSPServer :: String -> IO (Maybe (String, [String]))
findLSPServer ext = do
  case Map.lookup ext languageServerCommands of
    Nothing    -> pure Nothing
    Just (cmd, args) -> do
      found <- findExecutable cmd
      case found of
        Just path -> pure $ Just (path, args)
        Nothing   -> pure Nothing

-- | Convert file extension to LSP LanguageId string
languageIdFromExt :: String -> Text
languageIdFromExt ext = case ext of
  ".ts"    -> "typescript"
  ".tsx"   -> "typescriptreact"
  ".js"    -> "javascript"
  ".jsx"   -> "javascriptreact"
  ".py"    -> "python"
  ".go"    -> "go"
  ".rs"    -> "rust"
  ".c"     -> "c"
  ".cpp"   -> "cpp"
  ".h"     -> "c"
  ".hpp"   -> "cpp"
  ".java"  -> "java"
  ".cs"    -> "csharp"
  ".rb"    -> "ruby"
  ".hs"    -> "haskell"
  ".lhs"   -> "haskell"
  ".php"   -> "php"
  ".swift" -> "swift"
  ".kt"    -> "kotlin"
  ".kts"   -> "kotlin"
  ".scala" -> "scala"
  ".lua"   -> "lua"
  ".zig"   -> "zig"
  ".ex"    -> "elixir"
  ".exs"   -> "elixir"
  ".dart"  -> "dart"
  ".vue"   -> "vue"
  ".svelte" -> "svelte"
  ".nix"    -> "nix"
  ".json"   -> "json"
  _        -> "plaintext"

-- | Extract file extension including the dot
takeExtension :: FilePath -> String
takeExtension path =
  let reversed = reverse path
      ext = takeWhile (/= '.') reversed
  in if null ext then "" else '.' : reverse ext