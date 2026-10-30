#ifndef GRAPHOS_LEAN_CRITIQUE_H
#define GRAPHOS_LEAN_CRITIQUE_H

#include <stdint.h>

#define GRAPHOS_LEAN_CRITIQUE_ABI_VERSION 0
#define GRAPHOS_LEAN_CRITIQUE_VERSION "0.0.0-spike (AVI-1677 stage 0)"

/* Stage 0 feasibility spike: numeric score matching the Lean computation in
   Critique.lean and the Haskell `Graphos.Domain.Analysis.surpriseScore`.

   confX10        : confidence * 10, clamped to 0..10 (0..0.9.. handled as
                    buckets: <3, <8, otherwise)
   crossFiletype  : 1 when the endpoints live in different file categories
   crossComm      : 1 when the endpoints live in different communities
   inferred       : 1 when edgeRelation == Inferred
   returns        : the combined surprise score (non-negative) */
uint64_t graphos_surprise_score(uint8_t confX10,
                               uint8_t crossFiletype,
                               uint8_t crossComm,
                               uint8_t inferred);

#endif /* GRAPHOS_LEAN_CRITIQUE_H */
