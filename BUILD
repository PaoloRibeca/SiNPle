#!/bin/sh

#####################################################################
# You might need to change this line depending on your installation #
BIOCAMLIB_PATH=../BiOCamLib
#####################################################################

rm -f BiOCamLib
ln -s "$BIOCAMLIB_PATH" BiOCamLib

PROFILE="$1"
if [ -z "$PROFILE" ]; then
  PROFILE="dev"
fi

dune build --profile="$PROFILE" bin/SiNPle.exe

chmod 755 _build/default/bin/SiNPle.exe
if [ "$PROFILE" = "release" ]; then
  strip _build/default/bin/SiNPle.exe
fi

rm -f SiNPle
ln -s _build/default/bin/SiNPle.exe SiNPle

