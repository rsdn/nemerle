#!/usr/bin/python
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

import sys
import os
import time
import re
import getopt
import string
import codecs

from xml.utils import qp_xml

kill_prefix_rx = None
default_domain = "localhost"
exclude = []
users = { }
reloc = { }
max_join_delta = 3 * 60
list_format = False

date_rx = re.compile(r"^(\d+-\d+-\d+T\d+:\d+:\d+)")

def die(msg):
  sys.stderr.write(msg + "\n")
  sys.exit(1)

def attr(e, n):
  return e.attrs[("", n)]

def has_child(e, n):
  for c in e.children:
    if c.name == n: return 1
  return 0

def child(e, n):
  for c in e.children:
    if c.name == n: return c
  die("<%s> doesn't have <%s> child" % (e.name, n))
  
def convert_path(n):
  for src in reloc.keys():
    n = string.replace(n, src, reloc[src])
  if kill_prefix_rx != None:
    if kill_prefix_rx.search(n):
      n = kill_prefix_rx.sub("", n)
    else:
      return None
  if n.startswith("/"): n = n[1:]
  if n == "": n = "/"
  for pref in exclude:
    if n.startswith(pref):
      return None
  return n

def convert_user(u):
  if users.has_key(u):
    return users[u]
  else:
    return "%s <%s@%s>" % (u, u, default_domain)

def wrap_text_line(str, pref, width):
  ret = u""
  line = u""
  first_line = True
  for word in str.split():
    if line == u"":
      line = word
    else:
      if len(line + u" " + word) > width:
        if first_line:
          ret += line + u"\n"
          first_line = False
          line = word
        else:
          ret += pref + line + u"\n"
          line = word
      else:
        line += u" " + word
  if first_line:
    ret += line + u"\n"
  else:
    ret += pref + line + u"\n"
  return ret

def wrap_text(str, pref, width):
  if not list_format:
    return wrap_text_line(str,pref,width)
  else:
    items = re.split(r"\-\s+",str)
    ret = wrap_text_line(items[0],pref,width)
    for item in items[1:]:
      ret += pref + u"- " + wrap_text_line(item,pref+"  ",width)
    return ret

class Entry:
  def __init__(self, tm, rev, author, msg):
    self.tm = tm
    self.rev = rev
    self.author = author
    self.msg = msg
    self.beg_tm = tm
    self.beg_rev = rev

  def join(self, other):
    self.tm = other.tm
    self.rev = other.rev
    self.msg += other.msg

  def dump(self, out):
    if self.rev != self.beg_rev:
      out.write("%s [r%s-%s]  %s\n\n" % \
                          (time.strftime("%Y-%m-%d %H:%M +0000", time.localtime(self.beg_tm)), \
                           self.rev, self.beg_rev, convert_user(self.author)))
    else:
      out.write("%s [r%s]  %s\n\n" % \
                          (time.strftime("%Y-%m-%d %H:%M +0000", time.localtime(self.beg_tm)), \
                           self.rev, convert_user(self.author)))
    out.write(self.msg)
  
  def can_join(self, other):
    return self.author == other.author and abs(self.tm - other.tm) < max_join_delta

def process_entry(e):
  rev = attr(e, "revision")
  if has_child(e, "author"):
    author = child(e, "author").textof()
  else:
    author = "anonymous"
  m = date_rx.search(child(e, "date").textof())
  msg = child(e, "msg").textof()
  if m:
    tm = time.mktime(time.strptime(m.group(1), "%Y-%m-%dT%H:%M:%S"))
  else:
    die("evil date: %s" % child(e, "date").textof())
  paths = []
  for path in child(e, "paths").children:
    if path.name != "path": die("<paths> has non-<path> child")
    nam = convert_path(path.textof())
    if nam != None:
      if attr(path, "action") == "D":
        paths.append(nam + " (removed)")
      elif attr(path, "action") == "A":
        paths.append(nam + " (added)")
      else:
        paths.append(nam)
 
  if paths != []:
    return Entry(tm, rev, author, "\t* %s\n" % wrap_text(", ".join(paths) + ": " + msg, "\t  ", 65))

  return None

def process(fin, fout):
  parser = qp_xml.Parser()
  root = parser.parse(fin)

  if root.name != "log": die("root is not <log>")
  
  cur = None
  
  for logentry in root.children:
    if logentry.name != "logentry": die("non <logentry> <log> child")
    e = process_entry(logentry)
    if e != None:
      if cur != None:
        if cur.can_join(e):
          cur.join(e)
        else:
          cur.dump(fout)
          cur = e
      else: cur = e
        
  if cur != None: cur.dump(fout)

def usage():
  sys.stderr.write(\
"""Usage: %s [OPTIONS] [FILE]
Convert specified subversion xml logfile to GNU-style ChangeLog.

Options:
  -p, --prefix=REGEXP  set root directory of project (it will be striped off
                       from ChangeLog entries, paths outside it will be 
                       ignored)
  -x, --exclude=DIR    exclude DIR from ChangeLog (relative to prefix)
  -o, --output         set output file (defaults to 'ChangeLog')
  -d, --domain=DOMAIN  set default domain for logins not listed in users file
  -u, --users=FILE     read logins from specified file
  -F, --list-format    format commit logs with enumerated change list (items
                       prefixed by '- ')
  -r, --relocate=X=Y   before doing any other operations on paths, replace
                       X with Y (useful for directory moves)
  -D, --delta=SECS     when log entries differ by less then SECS seconds and
                       have the same author -- they are merged, it defaults
                       to 180 seconds
  -h, --help           print this information

Users file is used to map svn logins to real names to appear in ChangeLog.
If login is not found in users file "login <login@domain>" is used.

Example users file:
john    John X. Foo <jfoo@example.org>
mark    Marcus Blah <mb@example.org>

Typical usage of this script is something like this:

  svn log -v --xml | %s -p '/foo/(branches/[^/]+|trunk)' -u aux/users
  
Please send bug reports and comments to author:
  Michal Moskal <malekith@pld-linux.org>

""" % (sys.argv[0], sys.argv[0]))

def utf_open(name, mode):
  return codecs.open(name, mode, encoding="utf-8", errors="replace")

def process_opts():
  try:
    opts, args = getopt.gnu_getopt(sys.argv[1:], "o:u:p:x:d:r:d:D:Fh", 
                                   ["users=", "prefix=", "domain=", "delta=",
                                    "exclude=", "help", "output=", "relocate=",
                                    "list-format"])
  except getopt.GetoptError:
    usage()
    sys.exit(2)
  fin = sys.stdin
  fout = None
  global kill_prefix_rx, exclude, users, default_domain, reloc, max_join_delta, list_format
  for o, a in opts:
    if o in ("--prefix", "-p"):
      kill_prefix_rx = re.compile("^" + a)
    elif o in ("--exclude", "-x"):
      exclude.append(a)
    elif o in ("--help", "-h"):
      usage()
      sys.exit(0)
    elif o in ("--output", "-o"):
      fout = open(a, "w")
    elif o in ("--domain", "-d"):
      default_domain = a
    elif o in ("--users", "-u"):
      f = utf_open(a, "r")
      for line in f.xreadlines():
        w = line.split()
        if len(line) < 1 or line[0] == '#' or len(w) < 2: 
          continue
        users[w[0]] = " ".join(w[1:])
    elif o in ("--relocate", "-r"):
      (src, target) = a.split("=")
      reloc[src] = target
    elif o in ("--delta", "-D"):
      max_join_delta = int(a)
    elif o in ("--list-format", "-F"):
      list_format = True
    else:
      usage()
      sys.exit(2)
  if len(args) > 1:
    usage()
    sys.exit(2)
  if len(args) == 1:
    fin = open(args[0], "r")
  if fout == None:
    fout = utf_open("ChangeLog", "w")
  process(fin, fout)

if __name__ == "__main__":
  os.environ['TZ'] = 'UTC'
  try:
    time.tzset()
  except AttributeError:
    pass
  process_opts()
