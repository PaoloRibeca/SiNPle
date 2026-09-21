#!/usr/bin/env bash

set -e

# Usage:
#   bash BUILD [<profile>]      build .build/SiNPle with a dune profile -- dev
#                               (default), dev-static, release or release-static
#                               -- then run the characterization check
#   bash BUILD README.pdf       regenerate README.pdf from README.md
#   bash BUILD package [<ver>]  assemble releases/SiNPle-<ver>-<os>-<arch>.tar.xz
#   bash BUILD mac-begin        tag v<CURRENT> and push it, triggering the CI
#   bash BUILD mac-end          wait for it, download the macOS binaries, package
#
# A release, in order: write the new N.N.N into releases/CURRENT and commit it;
# bash BUILD release-static; bash BUILD README.pdf; commit; bash BUILD
# release-static once more, the version also carrying the commit-file count;
# bash BUILD package; bash BUILD mac-begin; bash BUILD mac-end.  Give package a
# <ver> only for a local package: it is not committed, so the tag mac-begin
# pushes would still carry the old one.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Everything below is relative to the tree this script sits in, and not to
# wherever it was invoked from.
cd "$ROOT"

# Everything that is not specific to SiNPle lives in the vendored BiOCamLib's
# tools/, which every repository of the family reaches the same way, so that
# none of them carries a second copy of it to drift.  What stays here is what
# SiNPle builds and the profiles it builds with.
TOOLS="$ROOT/BiOCamLib/tools"
[[ -d "$TOOLS" ]] \
  || { echo "BUILD: $TOOLS not found -- is the BiOCamLib submodule checked out?" >&2; exit 1; }

# Every dune invocation names the root explicitly.  Without it dune takes the
# OUTERMOST enclosing dune-project, which for a tree checked out inside another
# one -- NINJA vendors this repository, and a git worktree under .claude/ is
# another case -- is a different project altogether, and the build then either
# fails or silently builds the wrong tree.
DUNE=(dune build --root "$ROOT")

# Regenerate README.pdf from README.md: pandoc into self-contained HTML, then
# headless Chrome, through the stylesheet and figure handling the family
# shares.  Needs pandoc, gawk and chrome/chromium.
if [[ "${1:-}" == "README.pdf" ]]; then
  bash "$TOOLS/markdown-pdf" --root "$ROOT" --title SiNPle
  exit 0
fi

# Release packaging and the macOS CI live in tools/release, which takes the
# project name and reads what ships from releases/MANIFEST
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

PROFILE="${1:-dev}"
# A mistyped target would otherwise be handed to dune as a profile, after _build
# and .build had already been wiped
case "$PROFILE" in
  dev|dev-static|release|release-static) ;;
  *)
    echo "BUILD: unknown profile or target '$PROFILE'" >&2
    exit 1
    ;;
esac

# Emit version info, for SiNPle and for the BiOCamLib it vendors.  SiNPle's goes
# into its library, as SiNPle.Info, where the command and whatever else links the
# library find it; --open is what lets it reach Tools.Argv, the module being
# outside BiOCamLib.
bash "$TOOLS/stamp-version" --root "$ROOT" --out "$ROOT/lib/Info.ml" --open SiNPle
bash "$TOOLS/stamp-version" --root "$ROOT/BiOCamLib" --out "$ROOT/BiOCamLib/lib/Info.ml" \
  BiOCamLib AnnoTools Cophenetic FASTools NJ Octopus Parallel RC TREx Yggdrasill

# Always erase both build directories to ensure peace of mind: a build that
# fails must not leave the binary of an earlier one in .build, where 'package'
# would take it for current.  Stamping comes first, so that a tree without
# history fails before anything is removed.
rm -rf "$ROOT/_build" "$ROOT/.build"
mkdir "$ROOT/.build"

#FLAGS="--verbose"

"${DUNE[@]}" --profile="$PROFILE" bin/SiNPle.exe $FLAGS

cp "$ROOT/_build/default/bin/SiNPle.exe" "$ROOT/.build/SiNPle"

chmod 755 "$ROOT"/.build/*

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
  strip "$ROOT"/.build/*
  rm -rf "$ROOT/_build"
fi
