#!/bin/bash
svn up ../boot/lock.txt
if test -n $(tail -n 2 ../boot/lock.txt | grep locked | awk '{print $1}'); then 
  cp ../boot/lock.txt locktmp.txt
  echo "  unlocked, "$(date) >> ../boot/lock.txt
  cd ..
  if svn commit -m "Release lock. $1"; then
    echo OK: release lock succesful;
    rm -f locktmp.txt;
 else
   echo FAIL: something went wrong;
   mv -f locktmp.txt ../boot/lock.txt;
 fi
else echo FAIL: project is not locked; fi
