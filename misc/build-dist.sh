#!/bin/sh

set -xe

svn up
./configure
make dist

tarballs=`find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'`
latest=`echo "$tarballs" | sort | tail -1`
scp $tarballs lilith:/home/services/nemerle.org/download/
ssh lilith "cp /home/services/nemerle.org/download/$latest /home/services/nemerle.org/download/nemerle-latest.tar.gz"
