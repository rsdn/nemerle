#!/bin/bash
svn up ../boot/lock.txt
if test -n $(tail -n 2 ../boot/lock.txt | grep locked | awk '{print $1}'); then 
  echo "  unlocked "$(date) >> ../boot.txt
  cd ..
  if svn commit -m "Release lock. $1"; then
    echo OK: release lock succesful;
 else
   echo FAIL: something went wrong;
 fi
else echo FAIL: project is not locked; fi
