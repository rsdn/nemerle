#!/usr/bin/perl -p -i -e

# drop certain serial numbers from Portable.NET's ildasm output

s/\?L[0-9a-f]+/L/g;
s/(_N\S*?)[0-9]+([\s:\)])/$1$2/g;
s/header: [0-9a-f]+//g;

