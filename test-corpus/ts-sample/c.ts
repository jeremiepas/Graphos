// test-corpus/ts-sample/c.ts
// Defines `main`, which calls `verdict` defined in a.ts (cross-file call).
import { verdict } from "./a"

export function main(): number {
  return verdict(5)
}
