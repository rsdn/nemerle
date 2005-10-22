#!/bin/sh

set -x
set -e

top=../../../
tar=
for x in `ls $top/nemerle-*.tar.gz` ; do
  tar=$x
done
test -f $x || {
  echo "No tarball found ($top/nemerle-*.tar.gz)"
  exit 1
}
rm -rf dist
mkdir -p dist/bin
cp -f $top/tools/cs2n/*.dll dist/bin/
cp -f $top/tools/cs2n/*.exe dist/bin/
cp -f $top/tools/nemerlish/*.exe dist/bin/
cp -f $top/tools/nemerlish/*.dll dist/bin/
cp -f $top/tools/nant-task/*.dll dist/bin/
cp -f $top/tools/msbuild-task/*.dll dist/bin/
cp -f $top/tools/msbuild-task/*.targets dist/bin/
cp -f $top/ncc/out.stage3/*.exe dist/bin
cp -f $top/ncc/out.stage3/*.dll dist/bin
tar -C dist -zxf $tar
mv dist/nemerle-*/doc/html dist/
rm -rf dist/nemerle-*
rm -f dist/bin/true.exe
cp License.rtf dist
