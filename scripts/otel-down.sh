#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
COMPOSE_FILE="${PROJECT_ROOT}/docker-compose.otel.yaml"

echo "Stopping Graphos OTel observability stack and removing volumes..."
docker compose -f "${COMPOSE_FILE}" down -v

echo "Done."
