#!/usr/bin/perl -w -i -p
# Attempt convertion fun/method/field to static/non-static.

s/^(\s*)fun ([^\(])/${1}static $2/;
s/^(\s*)fun (\(')/${1}static $2/;
s/^(\s*)method /${1}/;
s/^(\s*)value /${1}static /;
s/^(\s*)field /${1}/;
