#!/usr/bin/perl
#
# Copyright (c) 2003 The University of Wroclaw.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#    1. Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#    3. The name of the University may not be used to endorse or promote
#       products derived from this software without specific prior
#       written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
# NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

$nem_comp = shift;
defined $nem_comp or $nem_comp = "ncc";

$cs_compiler = shift;
defined $cs_compiler or $cs_compiler = "mcs";
$cs_compiler =~ /^(csc|cscc|mcs)$/ or die "bad cs_compiler";

$runtime = "";
if ($cs_compiler eq "mcs") {
    $runtime = "mono --debug ";
}

$compiler = $runtime . "../$nem_comp.exe ../../lib/aliases.n ../../lib/core.n ../../lib/list.n";

sub xgrep($$)
{
  my ($re, $fn) = @_;
  my $res = 0;
  open(GR, "< $fn");
  while (<GR>) {
    /$re/ and $res = 1;
  }
  close(GR);
  return $res;
}

$any_oops = 0;

sub err($)
{
  my $m = shift;
  if ($no_newline) {
    print STDERR "\n";
    $no_newline = 0;
  }
  $oops = 1;
  print STDERR "$fn: $m\n";
}

FILE: while (<*.n>) {
  $fn = $_;
  %err = ();
  %warn = ();
  %ok = ();
  $line = 1;
  open(FH, "< $fn");
  while (<FH>) {
    chomp;
    m|// E:\s*(.*)$| and $err{$line} = $1;
    m|// W:\s*(.*)$| and $warn{$line} = $1;
    m|// OK\s*$| and $ok{$line} = 1;
    $line++;
  }
  close(FH);
  $oops = 0;
  $test = "norm";
  if (!xgrep("^BEGIN-OUTPUT", $fn)) {
    next FILE;
  }
  if (scalar (keys %err) != 0) {
    $test = "err ";
    next FILE;
  } elsif (scalar (keys %warn) != 0) {
    $test = "warn";
  }

  $any_errors = scalar (keys %err) + scalar (keys %warn);
  
  print STDERR "${test} : $fn... ";
  $no_newline = 1;
  
  $res = system("$compiler -texe -out:t.exe $fn > test.err 2>&1");

  if ($res && scalar (keys %err) == 0) {
    err("unxepected error exit status");
  }

  foreach (keys %err) {
    $line = $_;
    err("expected error: \"$err{$line}\" at line $line")
      unless (xgrep("^$fn:$line:[\\d:]* error:.*$err{$line}", "test.err"));
  }
  
  foreach (keys %warn) {
    $line = $_;
    err("expected warning: \"$warn{$line}\" at line $line")
      unless (xgrep("^$fn:$line:[\\d:]* warning:.*$warn{$line}", "test.err"));
  }

  foreach (keys %ok) {
    $line = $_;
    err("unexpected error at line $line")
      if (xgrep("^$fn:$line:", "test.err"));
  }

  unless ($any_errors) {
    open (IN, "<test.err") or die;
    while (<IN>) {
      chomp;
      s/\r//;
      /\.\.\.$/ and next;
      / debug: / and next;
      /^$/ and next;
      err("unexpected compiler output ($_)");
    }
    close(IN);
  }

  if ($res == 0) {
    print STDERR "C# ";
    $no_newline = 1;
    if (xgrep("^BEGIN-OUTPUT", $fn)) {
      $exp_out = "";
      $copy_mode = 0;
      open (F, "< $fn");
      while (<F>) {
        s/\r\n/\n/g;
        /^END-OUTPUT/ and $copy_mode = 0;
        $exp_out .= $_ if $copy_mode;
        /^BEGIN-OUTPUT/ and $copy_mode = 1;
      }
      close (F);
      $out = "";
      open(F, $runtime . "t.exe 2>&1 |");
      while (<F>) {
        s/\r\n/\n/g;
        $out .= $_;
      }
      close(F);
      unlink("t.exe");
      if ($out ne $exp_out) {
        err("bad testcase output");
	print STDERR "\nneeded:\n'$exp_out'\ngot:\n'$out'\n\n";
      }
    }
  }

  unlink("test.err");
  unlink("out.cs");
    
  if ($oops) {
    print STDERR "FAIL\n";
    $any_oops++;
  } else {
    print STDERR "OK\n";
  }
}

if ($any_oops) {
  print STDERR "ERROR: $any_oops tests failed.\n";
  exit 1;
} else {
  print STDERR "All OK.\n";
  exit 0;
}
