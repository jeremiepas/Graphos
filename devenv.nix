{ pkgs, lib, config, ... }:

let
  hp = pkgs.haskell.packages.ghc910;
  systemDeps = with pkgs; [
    zlib
    openssl
    poppler-utils
  ];
  tooling = with pkgs; [
    aider-chat
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
      exec = ''cabal update && cabal configure --enable-tests --flag dev -j4 && cabal build all -j4'';
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



    # Apply OpenSpec changes headlessly — never stops, runs all pending changes.
    # One opencode run per task (keeps context small, avoids overflow).
    # Run:  devenv tasks run openspec:apply
    # Stop: Ctrl-C or kill the process.
    "openspec:apply" = {
      exec = ''
        LOGFILE="/tmp/opencode-openspec-apply.log"
        echo "=== openspec:apply started at $(date) ===" > "$LOGFILE"

        # Find next change with undone tasks
        find_next_change() {
          for dir in ~/Documents/Graphos/openspec/changes/*/; do
            [ -f "$dir/tasks.md" ] || continue
            if grep -q '^\- \[ \]' "$dir/tasks.md" 2>/dev/null; then
              basename "$dir"
              return 0
            fi
          done
          return 1
        }

        while true; do
          CHANGE=$(find_next_change)
          if [ -z "$CHANGE" ]; then
            echo "=== No more changes with pending tasks. All done! ===" | tee -a "$LOGFILE"
            break
          fi

          echo "=== Starting change: $CHANGE ===" | tee -a "$LOGFILE"
          TASKNUM=0

          while true; do
            # Check if change still has undone tasks
            if ! grep -q '^\- \[ \]' ~/Documents/Graphos/openspec/changes/"$CHANGE"/tasks.md 2>/dev/null; then
              echo "=== Change $CHANGE: all tasks complete! ===" | tee -a "$LOGFILE"
              break
            fi

            TASKNUM=$((TASKNUM + 1))
            echo "--- $CHANGE task #$TASKNUM ---" | tee -a "$LOGFILE"

            # One opencode run: find first unchecked task, implement it, mark done
            opencode run \
              --model "executor/qwen3.8-executor" \
              --auto \
              "Read the file openspec/changes/$CHANGE/tasks.md. Find the first line that starts with '- [ ]' (unchecked task). Read its description. Implement that task using the edit, bash, read, and grep tools. Then edit tasks.md to change that '- [ ]' to '- [x]'. Stop after marking it done. Do NOT read any openspec context files — just tasks.md and the source files you need." \
              >> "$LOGFILE" 2>&1

            EXIT_CODE=$?
            echo "opencode exit=$EXIT_CODE" >> "$LOGFILE"

            # Never stop on errors — sleep and retry
            if [ $EXIT_CODE -ne 0 ]; then
              echo "opencode error (code $EXIT_CODE), retrying in 10s..." | tee -a "$LOGFILE"
              sleep 10
            else
              sleep 2
            fi
          done

          echo "=== Moving to next change ===" | tee -a "$LOGFILE"
        done

        echo "=== openspec:apply finished at $(date) ===" | tee -a "$LOGFILE"
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

  # gemma4 process: llama.cpp server hosting Gemma 4 26B A4B (long-running).
  # Requires $LLAMA_MODELS_DIR/gemma-4-26B-A4B-it-<QUANT>.gguf.
  # Run:  devenv up  (process starts automatically with ready-check)
  processes.gemma4 = {
    exec = ''
      QUANT=''${GEMMA_QUANT:-UD-IQ4_XS}
      MODEL="$LLAMA_MODELS_DIR/gemma-4-26B-A4B-it-$QUANT.gguf"
      if [ ! -f "$MODEL" ]; then
        echo "Model $MODEL not found - run: devenv tasks run download-gemma4" >&2
        exit 1
      fi
      MMPROJ_ARGS=()
      if [ "''${GEMMA_MMPROJ:-0}" = "1" ]; then
        MMPROJ_ARGS=(--mmproj "$LLAMA_MODELS_DIR/mmproj-F16.gguf")
      fi
      exec llama-server \
        -hf "unsloth/gemma-4-26B-A4B-it-GGUF:UD-Q5_K_S" \
        "''${MMPROJ_ARGS[@]}" \
        --host "''${LLAMA_HOST:-0.0.0.0}" \
        --port "''${GEMMA_PORT:-8081}" \
        --n-gpu-layers 20 \
        --n-cpu-moe "''${GEMMA_N_CPU_MOE:-0}" \
        --jinja \
        --tools all \
        --fit on --fit-ctx 32768 \
        --ctx-size "''${LLAMA_CTX_SIZE:-165536}" \
        --cache-type-k "''${LLAMA_CACHE_TYPE_K:-q8_0}" \
        --cache-type-v "''${LLAMA_CACHE_TYPE_V:-q8_0}" \
        --temperature 0.6 \
        --reasoning-budget 200 \
        --top_p 0.80 \
        --metrics
    '';

    ready = {
      exec = ''
        bash -c 'HOST="''${LLAMA_HOST:-localhost}"; PORT="''${GEMMA_PORT:-8081}"; curl -fsS "http://$HOST:$PORT/health" > /dev/null'
      '';
      initial_delay = 15;
      failure_threshold = 24; # ~2 min total with 5s interval
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
