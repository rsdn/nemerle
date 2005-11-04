#!/bin/sh
set -e

TMP=/tmp 
TEMP=/tmp
DIR=`dirname $0`
NANT=`grep "NANT " ./$DIR/../../../config.mak | sed 's/.*=//g'`
command=eval $NANT -t:net-2.0 -v
$command
version=$(grep '"msi.version"' msi.build | sed -e 's/.*value="//; s/".*//')
chmod 644 Nemerle.msi
mv Nemerle.msi nemerle-$version.msi
