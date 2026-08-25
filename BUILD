#!/usr/bin/env bash

set -e

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Everything that is not specific to SiNPle lives in the vendored BiOCamLib's
# tools/, which every repository of the family reaches the same way, so that
# none of them carries a second copy of it to drift.  What stays here is what
# SiNPle builds and the profiles it builds with.
TOOLS="$ROOT/BiOCamLib/tools"
[[ -d "$TOOLS" ]] \
  || { echo "BUILD: $TOOLS not found -- is the BiOCamLib submodule checked out?" >&2; exit 1; }

# Regenerate README.pdf from README.md.  Needs pandoc and chrome/chromium.
if [[ "${1:-}" == "README.pdf" ]]; then
  bash "$TOOLS/readme-pdf" --root "$ROOT" --title SiNPle
  exit 0
fi

# Release packaging and the macOS CI:
#   ./BUILD package [<ver>]   assemble releases/SiNPle-<ver>-<os>-<arch>.tar.xz
#   ./BUILD mac-begin         tag v<CURRENT> and push it, triggering the CI
#   ./BUILD mac-end           wait for it, download the macOS binaries, package
if [[ "${1:-}" == "package" ]]; then
  bash "$TOOLS/release" package "${2:-}" --root "$ROOT" --name SiNPle
  exit 0
fi

if [[ "${1:-}" == "mac-begin" ]]; then
  bash "$TOOLS/release" mac-begin --root "$ROOT"
  exit 0
fi

if [[ "${1:-}" == "mac-end" ]]; then
  bash "$TOOLS/release" mac-end --root "$ROOT" --name SiNPle
  exit 0
fi

PROFILE="$1"
if [[ "$PROFILE" == "" ]]; then
  PROFILE="dev"
fi

# Always erase build directory to ensure peace of mind
rm -rf _build

# Emit version info, for SiNPle and for the BiOCamLib it vendors.  SiNPle's
# module is Version and not Info because 'open BiOCamLib' in SiNPle.ml shadows a
# same-named module of this executable, which would leave the library's Info
# answering to the bare name; --open is what lets it reach Tools.Argv at all,
# being a module of the executable rather than of the library.
bash "$TOOLS/stamp-version" --root "$ROOT" --out "$ROOT/bin/Version.ml" --open SiNPle
bash "$TOOLS/stamp-version" --root "$ROOT/BiOCamLib" --out "$ROOT/BiOCamLib/lib/Info.ml" \
  BiOCamLib AnnoTools Cophenetic FASTools NJ Octopus Parallel RC TREx Yggdrasill

#FLAGS="--verbose"

dune build --profile="$PROFILE" bin/SiNPle.exe $FLAGS

rm -rf .build
mkdir .build

cp _build/default/bin/SiNPle.exe .build/SiNPle

chmod 755 .build/*

# The characterization check, run here so that what it checks is necessarily the
# binary just built: 'set -e' means a failed build never reaches this line, which
# is the whole point -- run by hand after a failed build it would have tested
# whatever .build happened to still hold, and reported four identical cases about
# code that was never compiled.
#
# An absent samtools SKIPS it, and says so on stderr rather than passing
# quietly: the macOS runner has no samtools, and a check that silently ran
# nothing is indistinguishable from one that passed.
if command -v samtools >/dev/null 2>&1; then
  bash "$ROOT/test/characterize"
else
  echo "BUILD: samtools not found -- the characterization check was SKIPPED, not passed" >&2
fi

if [[ "$PROFILE" == "release" || "$PROFILE" == "release-static" ]]; then
  strip .build/*
  rm -rf _build
fi
