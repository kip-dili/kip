#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

say() {
  printf "%s\n" "$*"
}

need_cmd() {
  command -v "$1" >/dev/null 2>&1
}


ensure_package_mgr() {
  if [[ "$OSTYPE" == darwin* ]]; then
    if ! need_cmd brew; then
      say "Homebrew not found. Please install Homebrew: https://brew.sh"
      exit 1
    fi
  else
    if ! need_cmd apt-get && ! need_cmd dnf && ! need_cmd pacman; then
      say "No supported package manager found. Please install Foma manually."
      exit 1
    fi
  fi
}

install_foma() {
  if need_cmd foma; then
    say "Foma is already installed."
    return
  fi
  say "Installing Foma..."
  if [[ "$OSTYPE" == darwin* ]]; then
    brew install foma
  elif need_cmd apt-get; then
    sudo apt-get update
    sudo apt-get install -y foma libfoma-dev
  elif need_cmd dnf; then
    sudo dnf install -y foma foma-devel
  elif need_cmd pacman; then
    sudo pacman -Sy --noconfirm foma
  else
    say "Could not install Foma automatically. Please install it manually."
    exit 1
  fi
}


install_stack() {
  if need_cmd stack; then
    say "Stack is already installed."
    return
  fi
  say "Installing Stack..."
  if [[ "$OSTYPE" == darwin* ]]; then
    brew install haskell-stack
  elif need_cmd apt-get; then
    sudo apt-get update
    sudo apt-get install -y haskell-stack
  elif need_cmd dnf; then
    sudo dnf install -y haskell-stack
  elif need_cmd pacman; then
    sudo pacman -Sy --noconfirm stack
  else
    say "Could not install Stack automatically. Please install it manually."
    exit 1
  fi
}

build_project() {
  say "Building the project with Stack..."
  (cd "$ROOT_DIR" && stack build)
}

main() {
  ensure_package_mgr
  install_foma
  install_stack
  build_project
  say "Setup complete. Open a new shell and run \"kip\"."
}

main "$@"
