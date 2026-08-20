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



    # Apply an OpenSpec change headlessly via opencode + hierarchical agent.
    # The EXECUTOR (GPU, 64k ctx, MTP spec decode) is the main model — it does
    # ALL tool calls (edit, bash, grep, read), reflection, and implementation.
    # The ORCHESTRATOR (CPU, 300k ctx) is only for long-context planning — the
    # executor can call it via curl POST to :8082/v1/chat/completions when it
    # needs to reason over the full conversation history.
    #
    # Run:  OPENSPEC_CHANGE=<change-name> devenv tasks run openspec:apply
    # Requires both processes to be up on localhost:
    #   devenv up qwen3-8-orchestrator qwen3-8-executor
    "openspec:apply" = {
      exec = ''
        CHANGE="''${OPENSPEC_CHANGE:-}"
        MAX_ITER="''${OPENSPEC_MAX_ITER:-50}"
        LOGFILE="''${OPENSPEC_LOG:-/tmp/opencode-openspec-apply.log}"
        if [ -z "$CHANGE" ]; then
          echo "openspec:apply: set OPENSPEC_CHANGE=<change-name> before starting." >&2
          echo "Available changes:" >&2
          openspec list >&2
          exit 1
        fi
        ITER=0
        while [ "$ITER" -lt "$MAX_ITER" ]; do
          ITER=$((ITER + 1))
          echo "=== Iteration $ITER/$MAX_ITER for change '$CHANGE' ===" | tee -a "$LOGFILE"

          # Check remaining tasks
          REMAINING=$(openspec status --change "$CHANGE" --json 2>/dev/null | jq '.progress.remaining // empty' 2>/dev/null)
          if [ -z "$REMAINING" ]; then
            REMAINING=$(openspec instructions apply --change "$CHANGE" --json 2>/dev/null | jq '.progress.remaining // 0' 2>/dev/null)
          fi
          echo "Remaining tasks: $REMAINING" | tee -a "$LOGFILE"
          if [ "$REMAINING" = "0" ]; then
            echo "All tasks complete for '$CHANGE'!" | tee -a "$LOGFILE"
            break
          fi

          # Run opencode with qwen3.8 orchestrator (CPU, 300k ctx)
          # chunkTimeout set to 600s in opencode.json to survive long prefill
          if [ "$ITER" -eq 1 ]; then
            opencode run \
              --model "orchestrator/qwen3.8-orchestrator" \
              --auto \
              "You are an autonomous agent. Apply the OpenSpec change '$CHANGE' using the openspec-apply-change skill. Steps: 1) Run openspec instructions apply --change \"$CHANGE\" --json 2) Read all context files 3) For each undone task: implement it using tools (edit, bash, read, grep), then mark it [x] in tasks.md 4) Do NOT ask for clarification — make decisions autonomously 5) Do NOT stop until all tasks are done or you hit a fatal error. Execute now." >> "$LOGFILE" 2>&1
          else
            opencode run \
              --model "orchestrator/qwen3.8-orchestrator" \
              --auto \
              --continue \
              "Continue autonomously. Pick up the next undone task from '$CHANGE' and implement it using tools. Mark it [x] in tasks.md. Do NOT ask for clarification. Do NOT stop until all tasks are done or you hit a fatal error." >> "$LOGFILE" 2>&1
          fi
          EXIT_CODE=$?
          echo "opencode exited with code $EXIT_CODE" | tee -a "$LOGFILE"

          if [ $EXIT_CODE -ne 0 ]; then
            echo "opencode error, stopping" | tee -a "$LOGFILE"
            break
          fi

          sleep 5
        done
        echo "=== Finished after $ITER iterations ===" | tee -a "$LOGFILE"
        openspec status --change "$CHANGE" | tee -a "$LOGFILE"
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
