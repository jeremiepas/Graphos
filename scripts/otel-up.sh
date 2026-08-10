#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
COMPOSE_FILE="${PROJECT_ROOT}/docker-compose.otel.yaml"

if ! command -v docker &>/dev/null; then
    echo "ERROR: docker is not installed" >&2
    exit 1
fi

if ! docker compose version &>/dev/null; then
    echo "ERROR: docker compose is not installed" >&2
    exit 1
fi

echo "Starting Graphos OTel observability stack..."
docker compose -f "${COMPOSE_FILE}" up -d

SERVICES=(otel-collector tempo loki prometheus grafana)
TIMEOUT=60
INTERVAL=5
ELAPSED=0

while [ ${ELAPSED} -lt ${TIMEOUT} ]; do
    ALL_HEALTHY=true
    echo ""
    echo "--- Status @ ${ELAPSED}s ---"
    for svc in "${SERVICES[@]}"; do
        STATUS=$(docker inspect --format='{{if .State.Health}}{{.State.Health.Status}}{{else}}{{.State.Status}}{{end}}' "graphos-${svc}" 2>/dev/null || echo "unknown")
        printf "%-20s %s\n" "${svc}" "${STATUS}"
        if [ "${STATUS}" != "healthy" ]; then
            ALL_HEALTHY=false
        fi
    done

    if [ "${ALL_HEALTHY}" = true ]; then
        echo ""
        echo "All services are healthy."
        echo "Grafana:       http://localhost:3000"
        echo "Tempo:         http://localhost:3200"
        echo "Loki:          http://localhost:3100"
        echo "Prometheus:    http://localhost:9099"
        echo "OTLP ingest:   http://localhost:4318"
        exit 0
    fi

    sleep "${INTERVAL}"
    ELAPSED=$((ELAPSED + INTERVAL))
done

echo ""
echo "ERROR: not all services became healthy within ${TIMEOUT}s" >&2
exit 1
