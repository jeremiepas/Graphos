#!/usr/bin/env bash
set -euo pipefail

# create-runner.sh — Deploy a GitHub Actions self-hosted runner on K3s.
#
# Usage:
#   scripts/create-runner.sh <runner-name> [repo-url]
#
# Creates under worker-github/:
#   deployment-<name>.yaml   — Kubernetes Deployment + Service
#   <name>-secret.yaml       — Kubernetes Secret for GITHUB_TOKEN
#   <name>-hpa.yaml          — HorizontalPodAutoscaler
#
# Args:
#   runner-name   — unique identifier for the runner (e.g. graphos-types)
#   repo-url      — GitHub repo URL to register runner against (default: ./)

RUNNER_NAME="${1:-}"
REPO_URL="${2:-.}"
WORKER_DIR="worker-github"

if [[ -z "$RUNNER_NAME" ]]; then
  echo "Usage: $0 <runner-name> [repo-url]" >&2
  exit 1
fi

# Sanitize: only lowercase alphanumeric + hyphens
if [[ ! "$RUNNER_NAME" =~ ^[a-z][a-z0-9-]*$ ]]; then
  echo "Error: runner-name must be lowercase alphanumeric with hyphens, starting with a letter" >&2
  exit 1
fi

mkdir -p "$WORKER_DIR"

# ── Deployment ───────────────────────────────────────────────────────────────
cat > "$WORKER_DIR/deployment-${RUNNER_NAME}.yaml" <<EOF
apiVersion: apps/v1
kind: Deployment
metadata:
  name: runner-${RUNNER_NAME}
  namespace: worker-github
  labels:
    app: runner-${RUNNER_NAME}
    runner: "${RUNNER_NAME}"
spec:
  replicas: 1
  selector:
    matchLabels:
      app: runner-${RUNNER_NAME}
      runner: "${RUNNER_NAME}"
  template:
    metadata:
      labels:
        app: runner-${RUNNER_NAME}
        runner: "${RUNNER_NAME}"
    spec:
      containers:
        - name: runner
          image: ghcr.io/actions/runner-actions-ubuntu-jammy:latest
          imagePullPolicy: IfNotPresent
          env:
            - name: ACTIONS_RUNNER_DEPENDENCY_TRACKING
              value: "false"
            - name: GITHUB_TOKEN
              valueFrom:
                secretKeyRef:
                  name: "${RUNNER_NAME}-secret"
                  key: github-token
            - name: RUNNER_GROUP
              value: "Default"
            - name: RUNNER_WORKDIR
              value: "/home/runner/work"
          volumeMounts:
            - name: data
              mountPath: /home/runner/_work
          resources:
            requests:
              memory: "2Gi"
              cpu: "1000m"
            limits:
              memory: "8Gi"
              cpu: "4000m"
      volumes:
        - name: data
          persistentVolumeClaim:
            claimName: runner-data-${RUNNER_NAME}
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: runner-data-${RUNNER_NAME}
  namespace: worker-github
spec:
  accessModes:
    - ReadWriteOnce
  storageClassName: standard
  resources:
    requests:
      storage: 50Gi
---
apiVersion: v1
kind: Service
metadata:
  name: runner-${RUNNER_NAME}
  namespace: worker-github
spec:
  selector:
    app: runner-${RUNNER_NAME}
  ports:
    - port: 80
      targetPort: 80
  type: ClusterIP
EOF

# ── Secret ───────────────────────────────────────────────────────────────────
cat > "$WORKER_DIR/${RUNNER_NAME}-secret.yaml" <<EOF
apiVersion: v1
kind: Secret
metadata:
  name: "${RUNNER_NAME}-secret"
  namespace: worker-github
type: Opaque
stringData:
  github-token: "${RUNNER_NAME}-token-placeholder"
EOF

# ── HPA ──────────────────────────────────────────────────────────────────────
cat > "$WORKER_DIR/${RUNNER_NAME}-hpa.yaml" <<EOF
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: runner-${RUNNER_NAME}
  namespace: worker-github
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: runner-${RUNNER_NAME}
  minReplicas: 1
  maxReplicas: 6
  metrics:
    - type: Resource
      resource:
        name: cpu
        target:
          type: Utilization
          averageUtilization: 70
    - type: Resource
      resource:
        name: memory
        target:
          type: Utilization
          averageUtilization: 80
EOF

echo "Created runner deployment for '${RUNNER_NAME}':"
echo "  ${WORKER_DIR}/deployment-${RUNNER_NAME}.yaml"
echo "  ${WORKER_DIR}/${RUNNER_NAME}-secret.yaml"
echo "  ${WORKER_DIR}/${RUNNER_NAME}-hpa.yaml"
