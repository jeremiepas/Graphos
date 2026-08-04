{ pkgs, lib, config, ... }:

let
  hp = pkgs.haskell.packages.ghc910;
  systemDeps = with pkgs; [
    zlib
    openssl
    poppler-utils
  ];
  tooling = with pkgs; [
    jq
    pyright
    python313Packages.pyyaml
    bun
    uv
    nixd
    vscode-langservers-extracted
  ];
in
{
  # Haskell language support with GHC 9.10
  languages.haskell.enable = true;
  languages.haskell.package = hp.ghc;
  languages.haskell.cabal.package = hp.cabal-install;
  languages.haskell.lsp.package = hp.haskell-language-server;

  # All packages: Haskell tooling + system deps + general tooling
  packages =
    [ hp.ghc
      hp.cabal-install
      hp.haskell-language-server
      hp.hpack
      hp.hspec-discover
    ]
    ++ systemDeps
    ++ tooling;

  # mgconsole script: remap host port 7688 -> container port 7687
  scripts.mgconsole.exec = ''
    # Remap host port 7688 → container port 7687 since docker-compose
    # maps 7688:7687. mgconsole runs inside the container.
    ARGS=()
    skip_next=false
    for arg in "$@"; do
      if $skip_next; then
        if [ "$arg" = "7688" ]; then
          ARGS+=("7687")
        else
          ARGS+=("$arg")
        fi
        skip_next=false
      elif [ "$arg" = "-port" ]; then
        skip_next=true
        ARGS+=("$arg")
      else
        ARGS+=("$arg")
      fi
    done
    exec docker exec -i graphos-memgraph mgconsole "$${ARGS[@]}"
  '';

  # Environment variables
  env.EXTRA_LIBRARY_PATH = lib.makeLibraryPath systemDeps;
  env.OPENCODE_EXPERIMENTAL_LSP_TOOL = "true";

  # Shell activation greeting, clean stale PATH entries
  enterShell = ''
    # Remove stale PATH entries that are no longer used
    export PATH="$(echo "$PATH" | tr ':' '\n' | grep -v '\.cache/.bun/bin' | grep -v '\.npm-global/bin' | tr '\n' ':' | sed 's/:$//')"

    echo "graphos dev shell"
    echo "  ghc:   $(ghc --version)"
    echo "  cabal: $(cabal --version)"
  '';
}