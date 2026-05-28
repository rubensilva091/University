#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${REPO_URL:-}" ]]; then
  echo "ERROR: REPO_URL is required (example: https://github.com/org/repo)"
  exit 1
fi

if [[ -z "${RUNNER_TOKEN:-}" ]]; then
  echo "ERROR: RUNNER_TOKEN is required"
  exit 1
fi

RUNNER_NAME="${RUNNER_NAME:-container-runner}"
RUNNER_WORKDIR="${RUNNER_WORKDIR:-_work}"
RUNNER_LABELS="${RUNNER_LABELS:-self-hosted,linux,X64}"

cleanup() {
  if [[ -f .runner ]]; then
    ./config.sh remove --unattended --token "${RUNNER_TOKEN}" || true
  fi
}

trap cleanup EXIT INT TERM

./config.sh \
  --url "${REPO_URL}" \
  --token "${RUNNER_TOKEN}" \
  --name "${RUNNER_NAME}" \
  --work "${RUNNER_WORKDIR}" \
  --labels "${RUNNER_LABELS}" \
  --unattended \
  --replace

./run.sh