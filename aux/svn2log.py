#!/usr/bin/python

import sys
import re
import getopt

from xml.utils import qp_xml

kill_prefix = ""
default_domain = "localhost"
exclude = []
users = { }

date_rx = re.compile(r"^(\d+-\d+-\d+)T(\d+:\d+:\d+)")

def die(msg):
  sys.stderr.write(msg + "\n")
  sys.exit(1)

def attr(e, n):
  return e.attrs[("", n)]

def child(e, n):
  for c in e.children:
    if c.name == n: return c
  die("<%s> doesn't have <%s> child" % (e.name, n))
  
def convert_path(n):
  if n.startswith(kill_prefix):
    n = n[len(kill_prefix):]
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

def wrap_text(str, pref, width):
  ret = ""
  line = ""
  first_line = True
  for word in str.split():
    if line == "":
      line = word
    else:
      if len(line + " " + word) > width:
        if first_line:
          ret += line + "\n"
          first_line = False
          line = word
        else:
          ret += pref + line + "\n"
          line = word
      else:
        line += " " + word
  if first_line:
    ret += line + "\n"
  else:
    ret += pref + line + "\n"
  return ret

def process_entry(out, e):
  rev = attr(e, "revision")
  author = child(e, "author").textof()
  m = date_rx.search(child(e, "date").textof())
  msg = child(e, "msg").textof()
  if m:
    date = m.group(1)
    time = m.group(2)
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
    out.write("%s %s [r%s]  %s\n\n" % \
                          (date, time, rev, convert_user(author)))
    out.write("\t* %s\n" % wrap_text(", ".join(paths) + ": " + msg, "\t  ", 65))

def process(fin, fout):
  parser = qp_xml.Parser()
  root = parser.parse(fin)

  if root.name != "log": die("root is not <log>")
    
  for logentry in root.children:
    if logentry.name != "logentry": die("non <logentry> <log> child")
    process_entry(fout, logentry)

def usage():
  sys.stderr.write(\
"""Usage: %s [OPTIONS] [FILE]
Convert specified subversion xml logfile to GNU-style ChangeLog.

Options:
  -p, --prefix=DIR     set root directory of project (it will be striped off
                       from ChangeLog entries, paths outside it will be 
                       ignored)
  -e, --exclude=DIR    exclude DIR from ChangeLog (relative to prefix)
  -o, --output         set output file (defaults to 'ChangeLog')
  -d, --domain=DOMAIN  set default domain for logins not listed in users file
  -u, --users=FILE     read logins from specified file
  -h, --help           print this information

Users file is used to map svn logins to real names to appear in ChangeLog.
If login is not found in users file "login <login@domain>" is used.

Example users file:
john    John X. Foo <jfoo@example.org>
mark    Marcus Blah <mb@example.org>

Typical usage of this script is something like this:

  svn log -v --xml | %s -p /foo/trunk -u aux/users
  
Please send bug reports and comments to author:
  Michal Moskal <malekith@pld-linux.org>

""" % (sys.argv[0], sys.argv[0]))
  
def process_opts():
  try:
    opts, args = getopt.gnu_getopt(sys.argv[1:], "o:u:p:x:d:h", 
                                   ["users=", "prefix=", "domain=", 
                                    "exclude=", "help", "output="])
  except getopt.GetoptError:
    usage()
    sys.exit(2)
  fin = sys.stdin
  fout = None
  global kill_prefix, exclude, users, default_domain
  for o, a in opts:
    if o in ("--prefix", "-p"):
      kill_prefix = a
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
      f = open(a)
      for line in f.xreadlines():
        w = line.split()
        if len(line) < 1 or line[0] == '#' or len(w) < 2: 
          continue
        users[w[0]] = " ".join(w[1:])
    else:
      usage()
      sys.exit(2)
  if len(args) > 1:
    usage()
    sys.exit(2)
  if len(args) == 1:
    fin = open(args[0])
  if fout == None:
    fout = open("ChangeLog", "w")
  process(fin, fout)

if __name__ == "__main__":
  process_opts()
