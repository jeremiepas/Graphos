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

  libPaths = pkgs'.lib.makeLibraryPath systemDeps;

in
pkgs'.mkShell {
  name = "graphos";

  packages = haskellDeps ++ systemDeps ++ tooling;

  LD_LIBRARY_PATH = libPaths;
  EXTRA_LIBRARY_PATH = libPaths;

  shellHook = ''
    export PATH="$HOME/.cache/.bun/bin:$PATH"
    export PATH="$HOME/.npm-global/bin:$PATH"

    echo "graphos dev shell"
    echo "  ghc:   $(ghc --version)"
    echo "  cabal: $(cabal --version)"
  '';
}
