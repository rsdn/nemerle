#!/bin/sh

set -xe

svn up
./configure
make dist

tarballs=`find -maxdepth 1 -mindepth 1 -name 'nemerle-[0-9]*.tar.gz'`
latest=`echo "$tarballs" | sort | tail -1`
scp $tarballs lilith:/home/services/httpd/html/download/
ssh lilith "cp /home/services/httpd/html/download/$latest /home/services/httpd/html/download/nemerle-latest.tar.gz"
