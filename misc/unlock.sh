#!/bin/bash
svn up ../boot/lock.txt
if ! test -z $(tail -n 2 ../boot/lock.txt | grep locked); then 
  echo "  unlocked "$(date) >> lock.txt
#  msg="Release lock. "$1
  cd ..
  if svn commit -m "Release lock. $1"; then
    echo OK: release lock succesful;
  else
    echo FAIL: something went wrong;
  fi
else echo FAIL: project is not locked; fi
