#!/usr/bin/perl

$verb = 0;
$para = undef;

sub flush_para()
{
  return unless defined $para;
  print "$para\n\n";
  $para = undef;
}

while (<>)
{
  chomp;
  if (/^\\/) {
    flush_para();
    /^\\begin\{alltt\}/ and $verb = 1;
    /^\\end\{alltt\}/ and $verb = 0;
    print "$_\n";
  } elsif (/^\s*$/) {
    flush_para();
  } elsif ($verb) {
    print "$_\n";
  } else {
    s/^\s+//;
    if (defined $para) {
      $para .= " $_";
    } else {
      $para = $_;
    }
  }
}
