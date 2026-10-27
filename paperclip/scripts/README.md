# scripts

Small helpers that are not part of the Haskell build — e.g. API queries,
housekeeping, or status wrappers. Each script should read `PAPERCLIP_*` env
vars and never hard-code the API key or URL.

- `status.sh` — thin wrapper to run `../status.mjs` from anywhere.
