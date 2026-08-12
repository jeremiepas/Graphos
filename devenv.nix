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
    openspec
    llama-cpp
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

  # CI tasks - runnable locally and in CI
  tasks = {
    "ci:build" = {
      exec = ''cabal configure --enable-tests --flag dev -j4 && cabal build all -j4'';
    };
    "ci:test" = {
      exec = "cabal test all";
      after = [ "ci:build@succeeded" ];
    };
    "ci:haddock" = {
      exec = "cabal haddock all";
      after = [ "ci:build@succeeded" ];
    };
    "ci:release-build" = {
      exec = "cabal configure --enable-tests && cabal build all";
    };
    "ci:release-test" = {
      exec = "cabal test all";
    };

    # OpenSpec PDCA orchestrator — drive every active change once.
    # Run:  devenv tasks run orchestrator:run
    # Override args via ORCHESTRATOR_ARGS (default: "--all --no-health-check").
    "orchestrator:run" = {
      exec = ''
        export ORCHESTRATOR_REPO_ROOT="''${PWD}"
        python3 ${./orchestrator/orchestrate.py} \
          ''${ORCHESTRATOR_ARGS:---all --no-health-check}
      '';
    };

    # Start the local llama.cpp server hosting Qwen 3.6 (long-running).
    # Run:  devenv tasks run llama:server
    # Requires LLAMA_MODEL=/path/to/qwen3.6.gguf.
    "llama:server" = {
      exec = ''
        MODEL="''${LLAMA_MODEL:-}"
        PORT="''${LLAMA_PORT:-8080}"
        HOST="''${LLAMA_HOST:-0.0.0.0}"
        if [ -z "$MODEL" ]; then
          echo "llama:server: set LLAMA_MODEL=/path/to/qwen3.6.gguf before starting." >&2
          exit 1
        fi
        exec llama-server \
          --model "$MODEL" \
          --host "$HOST" \
          --port "$PORT" \
          --ctx-size 100768 \
          --n-gpu-layers 99 \
          --parallel 1 \
          --metrics
      '';
    };
  };

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

  # OpenSpec PDCA orchestrator (opencode + Qwen 3.6 / llama.cpp)
  # Drive one change end-to-end:
  #   orchestrator <change-name>
  # Drive every active (non-archived) change serially:
  #   orchestrator --all
  scripts.orchestrator.exec = ''
    exec python3 ${./orchestrator/orchestrate.py} "$@"
  '';

  # Environment variables
  env.EXTRA_LIBRARY_PATH = lib.makeLibraryPath systemDeps;
  env.OPENCODE_EXPERIMENTAL_LSP_TOOL = "true";
  env.LLAMA_BASEURL = "http://localhost:8080/v1";
  env.ORCHESTRATOR_POLL_INTERVAL = "300";

  # Shell activation greeting, clean stale PATH entries
  enterShell = ''
    # Remove stale PATH entries that are no longer used
    export PATH="$(echo "$PATH" | tr ':' '\n' | grep -v '\.cache/.bun/bin' | grep -v '\.npm-global/bin' | tr '\n' ':' | sed 's/:$//')"

    echo "graphos dev shell"
    echo "  ghc:   $(ghc --version)"
    echo "  cabal: $(cabal --version)"
  '';
}