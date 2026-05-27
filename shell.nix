let
  nixpkgsRev = "nixpkgs-unstable";
  pkgs = builtins.getFlake "github:NixOS/nixpkgs/${nixpkgsRev}";
  system = builtins.currentSystem;
  pkgs' = pkgs.legacyPackages.${system};

  hp = pkgs'.haskell.packages.ghc910;

  haskellDeps = with hp; [
    ghc
    cabal-install
    haskell-language-server
    hpack
    hspec-discover
  ];

  systemDeps = with pkgs'; [
    zlib
    openssl
  ];

  tooling = with pkgs'; [
    jq
    pyright
    python313Packages.pyyaml
    bun
    uv
    nixd
    vscode-langservers-extracted
  ];

  mgconsole = pkgs'.writeShellScriptBin "mgconsole" ''
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
    exec docker exec -i graphos-memgraph mgconsole "''${ARGS[@]}"
  '';

  libPaths = pkgs'.lib.makeLibraryPath systemDeps;

in
pkgs'.mkShell {
  name = "graphos";

  packages = haskellDeps ++ systemDeps ++ tooling ++ [ mgconsole ];

  LD_LIBRARY_PATH = libPaths;
  EXTRA_LIBRARY_PATH = libPaths;
  OPENCODE_EXPERIMENTAL_LSP_TOOL = true;
  shellHook = ''
    export PATH="$HOME/.cache/.bun/bin:$PATH"
    export PATH="$HOME/.npm-global/bin:$PATH"

    echo "graphos dev shell"
    echo "  ghc:   $(ghc --version)"
    echo "  cabal: $(cabal --version)"
  '';
}
