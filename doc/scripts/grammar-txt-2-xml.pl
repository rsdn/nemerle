#!/usr/bin/perl -w

print <<EOF
<grammar xmlns:xi="http://www.w3.org/2001/XInclude">
EOF
;
while (<>) {
restart:
if (/^([a-z0-9_]+):/) {
print "</rule>" if defined $rule;
$rule = $1;
print "<rule name=\"$rule\">\n";
} elsif (/^\t[^\s]/) {
	chomp;
	s/&/&amp;/g;
	s/</&lt;/g;
	s/>/&gt;/g;
	$_ = " $_ ";
		s/"([^"]+)"/<terminal name="$1" \/>/g;
		s/(ID|STRING|STRING_LITERAL|NUMBER_LITERAL|TYVAR)/<special-terminal name="$1" \/>/g;
	for($i=0;$i<50;$i++) {
		s/\s\{\s/ <repeat> /;
		s/\s\}\s/ <\/repeat> /;
		s/\s\{\+\s/ <repeat-plus> /;
		s/\s\}\+\s/ <\/repeat-plus> /;
		s/\s\[\s/ <optional> /;
		s/\s\]\s/ <\/optional> /;
		s/\s([a-z0-9_]+)\s/ <non-terminal name="$1" \/> /;
	}	
	print "<rhs><body>$_</body>\n";
	while (<>) {
		/^\s*$/ or last;
	}
	if (s/^\t\t//) {
		print "<desc>$_";
		while (<>) {
			/(^\t[^\t])|(^[^\t])/ and do {
			  print "</desc></rhs>\n";
			  goto restart;
			};
			s/^\t\t//;
			print;
		}
	} else { print "</rhs>\n"; goto restart; }
	
} elsif (/^#/ || /^\s*$/) {
} elsif (/^\s*<comment>/) {
	print;
	while(<>) {
		print;
		/<\/comment>/ and last;
	}
	defined $_ or die "runaway comment";
} else { die "syntax error"; }
}
print "</rule></grammar>\n";
