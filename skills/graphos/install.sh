#!/usr/bin/env bash
#
# Install the graphos CLI, the shared graphos config and the Claude Code skill.
#
# Usage: ./install.sh [--version vX.Y.Z] [--bin-dir DIR] [--force-config] [--skip-binary]
#
# - Binary: downloads the GitHub release, verifies its sha256, and patches the
#   ELF interpreter when the release (built on Nix) cannot start as-is.
# - Config: installs ~/.config/graphos/graphos.yaml; an existing file is kept
#   unless --force-config (then it is backed up first).
# - Skill: copies this folder to ~/.claude/skills/graphos (previous copy moved
#   to ~/.claude/skills-backup/).
# - Optional tools used by the skill's recipes: jq.

set -euo pipefail

readonly REPO="jeremiepas/Graphos"
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly STAMP="$(date +%Y%m%d%H%M%S)"

version="v0.1.3"
bin_dir="${HOME}/.local/bin"
force_config=0
skip_binary=0
tmp=""

log() { printf '[graphos-install] %s\n' "$*"; }
err() { printf '[graphos-install] ERROR: %s\n' "$*" >&2; }

while (($#)); do
  case "$1" in
    --version) version="$2"; shift 2 ;;
    --bin-dir) bin_dir="$2"; shift 2 ;;
    --force-config) force_config=1; shift ;;
    --skip-binary) skip_binary=1; shift ;;
    -h|--help) sed -n '2,15p' "$0"; exit 0 ;;
    *) err "unknown argument: $1"; exit 2 ;;
  esac
done

#######################################
# Download, verify and (if needed) patch the release binary.
#######################################
install_binary() {
  if [[ "$(uname -s)" != "Linux" || "$(uname -m)" != "x86_64" ]]; then
    err "no prebuilt binary for $(uname -s)/$(uname -m)."
    err "build from source: git clone https://github.com/${REPO} && cd Graphos && cabal install exe:graphos (GHC 9.10)"
    return 1
  fi

  local asset url
  tmp="$(mktemp -d)"
  trap 'rm -rf "${tmp}"' EXIT
  asset="graphos-linux-x86_64"
  url="https://github.com/${REPO}/releases/download/${version}"

  log "downloading ${asset} ${version}"
  curl -fsSL -o "${tmp}/${asset}" "${url}/${asset}"
  curl -fsSL -o "${tmp}/${asset}.sha256" "${url}/${asset}.sha256"
  (cd "${tmp}" && sha256sum -c "${asset}.sha256" >/dev/null) \
    || { err "sha256 mismatch"; return 1; }
  chmod +x "${tmp}/${asset}"

  if ! "${tmp}/${asset}" lservers >/dev/null 2>&1; then
    # The release is linked against /nix/store paths. On a regular distro,
    # point it at the system loader and drop the Nix rpath.
    local loader="/lib64/ld-linux-x86-64.so.2"
    if [[ ! -e "${loader}" ]]; then
      err "binary cannot start and ${loader} is missing (NixOS without nix-ld?)."
      err "enable programs.nix-ld, or build from source with cabal."
      return 1
    fi
    if ! command -v patchelf >/dev/null 2>&1; then
      err "patchelf is required to adapt the binary to this system."
      err "install it, e.g.: sudo apt-get install -y patchelf libgmp10 libffi8 libnuma1 libelf1 libdw1"
      return 1
    fi
    log "patching ELF interpreter for this distro"
    patchelf --set-interpreter "${loader}" --remove-rpath "${tmp}/${asset}"
    local missing
    missing="$(ldd "${tmp}/${asset}" | awk '/not found/ {print $1}' | tr '\n' ' ')"
    if [[ -n "${missing}" ]]; then
      err "missing shared libraries: ${missing}"
      err "Debian/Ubuntu: sudo apt-get install -y libgmp10 libffi8 libnuma1 libelf1 libdw1 (t64 variants on 24.04)"
      return 1
    fi
    "${tmp}/${asset}" lservers >/dev/null 2>&1 \
      || { err "patched binary still fails to start"; return 1; }
  fi

  mkdir -p "${bin_dir}"
  install -m 0755 "${tmp}/${asset}" "${bin_dir}/graphos"
  log "installed ${bin_dir}/graphos"
  case ":${PATH}:" in
    *":${bin_dir}:"*) ;;
    *) log "WARNING: ${bin_dir} is not on PATH; add: export PATH=\"${bin_dir}:\$PATH\"" ;;
  esac
}

#######################################
# Install the global graphos config, keeping an existing one by default.
#######################################
install_config() {
  local dir="${XDG_CONFIG_HOME:-${HOME}/.config}/graphos"
  local target="${dir}/graphos.yaml"
  mkdir -p "${dir}"
  if [[ -e "${target}" && "${force_config}" -eq 0 ]]; then
    log "kept existing ${target} (use --force-config to replace; template: config/graphos.global.yaml)"
    return 0
  fi
  if [[ -e "${target}" ]]; then
    cp "${target}" "${target}.bak-${STAMP}"
    log "backed up ${target} -> ${target}.bak-${STAMP}"
  fi
  cp "${SCRIPT_DIR}/config/graphos.global.yaml" "${target}"
  log "installed ${target}"
}

#######################################
# Install the skill into Claude Code's user skills directory.
#######################################
install_skill() {
  local target="${HOME}/.claude/skills/graphos"
  if [[ "$(cd "${SCRIPT_DIR}" && pwd -P)" == "$(cd "${target}" 2>/dev/null && pwd -P)" ]]; then
    log "skill already in place at ${target}"
    return 0
  fi
  if [[ -e "${target}" ]]; then
    # Back up OUTSIDE ~/.claude/skills: a copy left there loads as a second skill.
    local backup="${HOME}/.claude/skills-backup/graphos-${STAMP}"
    mkdir -p "$(dirname "${backup}")"
    mv "${target}" "${backup}"
    log "backed up previous skill -> ${backup}"
  fi
  mkdir -p "$(dirname "${target}")"
  cp -R "${SCRIPT_DIR}" "${target}"
  log "installed skill at ${target}"
}

main() {
  if [[ "${skip_binary}" -eq 0 ]]; then
    install_binary
  fi
  install_config
  install_skill
  local bin
  bin="$(command -v graphos || echo "${bin_dir}/graphos")"
  if [[ -x "${bin}" ]]; then
    "${bin}" lservers >/dev/null 2>&1 && log "check: ${bin} runs OK"
  fi
  command -v jq >/dev/null 2>&1 || log "note: install jq for the skill's graph.json recipes"
  log "done. Restart Claude Code (or start a new session) to load the skill."
}

main "$@"
