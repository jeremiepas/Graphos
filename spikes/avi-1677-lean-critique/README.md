# AVI-1677 Stage 0 feasibility spike

Lean 4 computation mirroring the numeric arm of
`Graphos.Domain.Analysis.surpriseScore` (see `src/Graphos/Domain/Analysis.hs`),
compiled with `@[export]` and consumed from GHC via `foreign import ccall`,
as a compile-time bridge (not a runtime dependency on a Lean interpreter).

## ABI (Stage 0, experimental)

| param            | type   | meaning                                                |
| ---------------- | ------ | ------------------------------------------------------ |
| `confX10`        | UInt8  | confidence * 10, scored as `<3`, `<8`, otherwise       |
| `crossFiletype`  | UInt8  | 1 when endpoint file categories differ                 |
| `crossComm`      | UInt8  | 1 when endpoints belong to different communities       |
| `inferred`       | UInt8  | 1 when `edgeRelation == Inferred`                      |
| return           | UInt64 | `confBonus + 2*crossFiletype + 1*crossComm + semBonus` |

Reason strings remain on the Haskell side for now (Stage 2 will freeze the
error/ownership contract and decide whether to move them).

## Build (NixOS)

```
cd spikes/avi-1677-lean-critique
make            # uses the Nix-provided Lean 4.29 toolchain
make run        # runs the fixture assertions
```

On an elan-based box, override `LEAN_PREFIX`, e.g.

```
make LEAN_PREFIX=$HOME/.elan/toolchains/leanprover--lean4---v4.14.0
```

## Linking into Graphos (Cabal note)

This is the recipe Stage 4/5 will automate. From a Cabal package:

```cabal
executable graphos
  ...
  Includes:           graphos_lean_critique.h
  Include-dirs:       spikes/avi-1677-lean-critique/include
  Extra-libraries:    graphoslean lean Init Std leanrt libleancpp libleanrt
  Extra-lib-dirs:     <path to the compiled .a>
```

At compile time the Lean side produces `Critique.o` (via `lean -c` then
`leanc -c`); archive it (`ar rcs libgraphoslean.a Critique.o`) and put it on
`Extra-lib-dirs`. Linker inputs reported by `leanc --print-ldflags` are the
system Lean runtime: `libInit`, `libStd`, `libLean`, `libleanrt`, plus
`libuv`, `gmp`, `stdc++`, `m`, `dl`, `pthread`.

The public FFI adapter (Stage 5) will instead call a small wrapper such as

```haskell
foreign import ccall "graphos_surprise_score"
  c_surpriseScore :: Word8 -> Word8 -> Word8 -> Word8 -> Word64
```

and keep the existing Haskell implementation as the reference fallback.
