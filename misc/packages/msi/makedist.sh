#!/bin/sh

set -x

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
cp $top/boot/*.exe dist/bin
cp $top/boot/*.dll dist/bin
tar -C dist -zxf $tar
mv dist/nemerle-*/doc/html dist/
rm -rf dist/nemerle-*
rm -f dist/bin/true.exe
cp License.rtf dist
