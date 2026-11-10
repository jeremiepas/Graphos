# Security Policy

## Supported versions

Graphos is under active development; security fixes are applied to the
latest release on `main`.

| Version | Supported |
| ------- | --------- |
| latest `main` / newest release | ✅ |
| older releases | ❌ |

## Reporting a vulnerability

**Please do not report security vulnerabilities through public GitHub issues,
discussions, or pull requests.**

Use **GitHub's private vulnerability reporting** instead:

1. Go to the [Security tab](https://github.com/jeremiepas/Graphos/security)
   of the repository.
2. Select **Report a vulnerability**.
3. Describe the issue, affected versions, and reproduction details.

Please include as much of the following as you can:

- Type of issue (e.g. path traversal, injection, unsafe deserialization)
- Full paths of source files related to the issue
- Location of the affected source code (tag/commit/branch)
- Step-by-step reproduction, proof-of-concept, or exploit code
- Impact, including how an attacker might exploit it

You should receive a response within a few days. If the issue is confirmed, a
patch will be prepared for the latest release and, where applicable, a GitHub
security advisory published with credit to the reporter.

## Scope

- The Graphos CLI and pipeline (extraction, build, cluster, infer, analysis,
  export)
- The MCP server shipped with Graphos
- CI and build configuration

Out of scope: vulnerabilities in third-party dependencies (report those
upstream), and issues requiring an already-compromised host.