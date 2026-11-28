// test-corpus/ts-sample/b.ts
// Defines `wrap`, which calls `verdict` defined in a.ts (cross-file call).
import { verdict } from "./a"

export function wrap(x: number): number {
  return verdict(x)
}
