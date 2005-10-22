#!/bin/sh

TMP=/tmp 
TEMP=/tmp
DIR=`dirname $0`
NANT=`grep "NANT " ./$DIR/../../../config.mak | sed 's/.*=//g'`
command=eval $NANT -t:net-2.0 -v
$command 
