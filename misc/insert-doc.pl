#!/usr/bin/perl -w
#
# Read dump.xml and Nemerle files given as arguments. Add <documentation/> 
# entries to dump.xml and save result as dump-out.xml.
#
# TODO:
#  - rewrite in Nemerle.
#  - use some internal syntax like [foo] for <verbatim>foo</verbatim>
#

%entries = ();
%max_line = ();

open(F, "< dump.xml") or die;
while (<F>) {
  if (/file="([^"]+)"\s+line="(\d+)"/) {
    $max_line{$1} = $2 if (!defined $max_line{$1} or $max_line{$1} < $2);
    $entries{"$1:$2"} = "";
  }
}
close(F);

foreach $f (@ARGV) {
  open(F, "<$f") or die;
  $line = 0;
  $in_comment = 0;
  $comment_val = "";
  while (<F>) {
    $line++;
    if (/^\s*\(\*\*(.*)/) {
      $in_comment = 1;
      $comment_val = $1;
    } elsif (/(.*)\*\)/) {
      $comment_val .= " $1" if ($in_comment);
      $in_comment = 0;
      for($i=$line;$i<=$max_line{$f};$i++) {
        if (defined $entries{"$f:$i"}) {
	  $entries{"$f:$i"} = $comment_val;
	}
      }
    } elsif ($in_comment) {
      $comment_val .= " $_";
    }
  }
  close(F);
}

open(F, "< dump.xml") or die;
open(O, "> dump-doc.xml") or die;
while (<F>) {
  print O $_;
  if (/file="([^"]+)"\s+line="(\d+)"/) {
    $e = $entries{"$1:$2"};
    if ($e ne "") {
      print O "<documentation>\n$e\n</documentation>\n";
    }
  }
}
close(O);
close(F);
