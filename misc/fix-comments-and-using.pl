#!/usr/bin/perl -w -i -p

s/^(\s*)open(\s)/$1using$2/;
next if /"/; 
s/\(\*/\/\*/g;
s/\*\)/\*\//g;
