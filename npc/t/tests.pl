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


$compiler = "../npc";
$cs_compiler = shift;
defined $cs_compiler or $cs_compiler = "cscc";
$cs_compiler =~ /^(cscc|mcs)$/ or die "bad cs_compiler";

-x $compiler or die "cannot find $compiler";

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

while (<*.n>) {
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
  if (scalar (keys %err) != 0) {
    $test = "err ";
  } elsif (scalar (keys %warn) != 0) {
    $test = "warn";
  }

  $any_errors = scalar (keys %err) + scalar (keys %warn);
  
  print STDERR "${test} : $fn... ";
  $no_newline = 1;
  
  $res = system("$compiler $fn 2> test.err");

  if ($res && scalar (keys %err) == 0) {
    err("unxepected error exit status");
  }
  
  foreach (keys %err) {
    $line = $_;
    err("expected error: \"$err{$line}\" at line $line")
      unless (xgrep("^$fn:$line: error:.*$err{$line}", "test.err"));
  }
  
  foreach (keys %warn) {
    $line = $_;
    err("expected warning: \"$warn{$line}\" at line $line")
      unless (xgrep("^$fn:$line: warning:.*$warn{$line}", "test.err"));
  }

  foreach (keys %ok) {
    $line = $_;
    err("unexpected error at line $line")
      if (xgrep("^$fn:$line: ", "test.err"));
  }

  err("unexpected compiler output")
    if (!$any_errors && -s "test.err");
    
  if ($res == 0) {
    print STDERR "C# ";
    $no_newline = 1;
    if (xgrep("^BEGIN-OUTPUT", $fn)) {
      $exp_out = "";
      $copy_mode = 0;
      open (F, "< $fn");
      while (<F>) {
        /^END-OUTPUT/ and $copy_mode = 0;
        $exp_out .= $_ if $copy_mode;
        /^BEGIN-OUTPUT/ and $copy_mode = 1;
      }
      close (F);
      if ($cs_compiler eq "cscc") {
        $res = system("cscc -o t.exe $fn.cs >/dev/null 2>&1");
	err("cscc failed on $fn.cs") if $res;
        $il_run = "ilrun";
      } elsif ($cs_compiler eq "mcs") {
        $res = system("mcs -out:t.exe -target:exe $fn.cs >/dev/null 2>&1");
	err("mcs failed on $fn.cs") if $res;
        $il_run = "mono";
      } else { die }
      $out = "";
      open(F, "$il_run t.exe 2>&1 |");
      while (<F>) {
        $out .= $_;
      }
      close(F);
      unlink("t.exe");
      if ($out ne $exp_out) {
        err("bad testcase output");
	print STDERR "\nneeded:\n$exp_out\ngot:$out\n\n";
      }
    } else {
      if ($cs_compiler eq "cscc") {
        $res = system("cscc -c -o /dev/null $fn.cs >/dev/null 2>&1");
	err("cscc failed on $fn.cs") if $res;
      } elsif ($cs_compiler eq "mcs") {
        $res = system("mcs -out:dev_null.o -target:module $fn.cs >/dev/null 2>&1");
	err("mcs failed on $fn.cs") if $res;
	unlink("dev_null.o");
      } else { die }
    }
  }

  unlink("test.err");
  unlink("$fn.cs");
    
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
