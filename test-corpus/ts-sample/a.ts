// test-corpus/ts-sample/a.ts
// Defines `verdict` (called cross-file from b.ts and c.ts) and
// `sameFileCaller`, which calls `verdict` within this same file.
export function verdict(x: number): number {
  return x * 2
}

function sameFileCaller(value: number): number {
  return verdict(value + 1)
}
