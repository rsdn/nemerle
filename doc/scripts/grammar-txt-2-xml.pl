#!/usr/bin/perl -w
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
# NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

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
