#!/bin/sh

top=../../../
rm -rf dist
mkdir -p dist/{bin,html}
cp $top/boot/*.{exe,dll} dist/bin
make -C $top/doc www
tar zxf $top/doc/nemerle-web.tar.gz -C dist/html
rm -r dist/html/{*.{gz,ps},course,styles/the-nemerlies.jpg}
cp License.rtf dist
