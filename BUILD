#!/usr/bin/env bash

set -e

PROFILE="$1"
if [[ "$PROFILE" == "" ]]; then
  PROFILE="dev"
fi

# Always erase build directory to ensure peace of mind
rm -rf _build

rm -rf build
mkdir build

# Emit version info for both BiOCamLib and SiNPle
cd BiOCamLib && echo -e "include (\n  struct\n    let info = {\n      Tools.Argv.name = \"BiOCamLib\";\n      version = \"$(git log --pretty=format: --name-only | awk '{if ($0!="") print}' | wc -l)\";\n      date = \"$(date -d "@$(git log -1 --format="%at")" +%d-%b-%Y)\"\n    }\n  end\n)" > lib/Info.ml && cd ..

#FLAGS="--verbose"

dune build --profile="$PROFILE" bin/SiNPle.exe

mv _build/default/bin/SiNPle.exe build/SiNPle

chmod 755 build/*

if [[ "$PROFILE" == "release" ]]; then
  strip build/*
  rm -rf _build
fi

