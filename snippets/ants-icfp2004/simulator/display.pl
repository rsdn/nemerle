#!/usr/bin/perl

while (<>) {
  /^Dump/ and do { $dummy = <STDIN>; print "\e[2J\e[1;1H"; };
  print;
}
