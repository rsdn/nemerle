#!/usr/bin/env bash

if [ $# -eq 0 ]; then
  echo Parameters required.
  echo Example 1: /p:TargetFrameworkVersion=v3.5 /t:DevBuildFull /p:Configuration=Release  /tv:4.0
  echo Example 2: /p:TargetFrameworkVersion=v4.5 /t:Stage4 /p:Configuration=Release /tv:4.0
else
  xbuild NemerleAll-Mono.nproj $*
fi