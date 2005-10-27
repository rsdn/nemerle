#!/usr/bin/perl -i

while (<>) {
  chomp;
  next if (/^\s*[{}]\s*$/);
  s/\s*{\s*$//;
  s/^(\s*)}\s*/$1/;
  s/;\s*$//;
  s/^(\s*\[[A-Z].*\])\s*$/$1 \\/;
  print "$_\n";
}
