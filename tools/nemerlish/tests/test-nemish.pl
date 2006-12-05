#!/usr/bin/perl

$err = 0;
$engine = shift;

sub one_test($) {
  $filename = shift;
  open(T, "< $filename");
  while (<T>) {
    /BEGIN-OUTPUT/ and last;
  }
  
  print STDERR "$filename ... ";
  
  open(M, "$engine nemish.exe < $filename |");
  $lerr = 0;
  while (<M>) {
    s/^- ([=-]\s*)*//;
    /^\s*$/ and next;
    /^(Please|Welcome|Type) / and next;
    $act = $_;
    $exp = <T>;
    chomp $act;
    chomp $exp;
    if ($act ne $exp) {
      print "\nExpected:\n$exp\ngot:\n$act\n";
      $lerr++;
    }
  }
  $l = <T>;
  if ($l =~ /END-OUTPUT/) { } else {
    $lerr++;
    print "\nExpected:\n$l\n";
  }

  if ($lerr == 0) { print STDERR "OK\n"; }
  else { $err++; print STDERR "FAIL\n"; }
}

foreach (@ARGV) {
  one_test($_);
}

if ($err != 0) {
  print STDERR "There were $err ERRORS!\n";
  exit 1;
} else {
  print STDERR "All OK\n";
  exit 0;
}
