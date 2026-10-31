#ifndef GRAPHOS_LEAN_CRITIQUE_H
#define GRAPHOS_LEAN_CRITIQUE_H

#include <stdint.h>

#define GRAPHOS_LEAN_CRITIQUE_ABI_VERSION 0
#define GRAPHOS_LEAN_CRITIQUE_VERSION "0.0.0-spike (AVI-1677 stage 0)"

/* Error codes (Stage 3/4 may extend; v0 itself does not fail). */
#define GRAPHOS_LEAN_OK 0
#define GRAPHOS_LEAN_ERR_INVALID_ARGUMENT 1
#define GRAPHOS_LEAN_ERR_VERSION_MISMATCH 2
#define GRAPHOS_LEAN_ERR_OUT_OF_MEMORY 3

/* Reason mask bits (future, non-breaking addition reserved by contract v0). */
#define GRAPHOS_LEAN_REASON_CROSSES_FILE_TYPES   (1u << 0)
#define GRAPHOS_LEAN_REASON_SEPARATE_COMMUNITIES (1u << 1)
#define GRAPHOS_LEAN_REASON_INFERRED_CONNECTION  (1u << 2)

/* Stage 0 feasibility spike: numeric score matching the Lean computation in
   Critique.lean and the Haskell `Graphos.Domain.Analysis.surpriseScore`.

   confX10        : confidence * 10, clamped to 0..10 (0..0.9.. handled as
                    buckets: <3, <8, otherwise)
   crossFiletype  : 1 when the endpoints live in different file categories
   crossComm      : 1 when the endpoints live in different communities
   inferred       : 1 when edgeRelation == Inferred
   returns        : the combined surprise score (non-negative) */
uint32_t graphos_lean_critique_abi_version(void);

/* v0 score predicate (see Critique.lean). */
uint64_t graphos_surprise_score(uint8_t confX10,
                               uint8_t crossFiletype,
                               uint8_t crossComm,
                               uint8_t inferred);

#endif /* GRAPHOS_LEAN_CRITIQUE_H */
