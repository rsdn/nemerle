#!/usr/bin/python

import sys
import re

from xml.utils import qp_xml

kill_prefix = "/nemerle/trunk"
users = { 'malekith' : 'Michal Moskal <malekith@pld-linux.org>' }

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
  if n.startswith(kill_prefix): n = n[len(kill_prefix):]
  if n.startswith("/"): n = n[1:]
  if n == "": n = "/"
  return n

def convert_user(u):
  if users.has_key(u):
    return users[u]
  else:
    return "%s <%s@nemerle.org>" % (u, u)

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

def process_entry(e):
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
    if attr(path, "action") == "D":
      paths.append(nam + " (removed)")
    elif attr(path, "action") == "A":
      paths.append(nam + " (added)")
    else:
      paths.append(nam)
  
  out = sys.stdout
  out.write("%s %s [r%s]  %s\n\n" % \
                        (date, time, rev, convert_user(author)))
  out.write("\t* %s\n" % wrap_text(", ".join(paths) + ": " + msg, "\t  ", 65))
      
parser = qp_xml.Parser()
root = parser.parse(sys.stdin)

if root.name != "log": die("root is not <log>")
  
for logentry in root.children:
  if logentry.name != "logentry": die("non <logentry> <log> child")
  process_entry(logentry)
