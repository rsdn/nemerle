#!/bin/bash
svn up ../boot/lock.txt
if test $(tail -n 1 ../boot/lock.txt | awk '{print $1 }';) = unlocked; then 
  echo "" >> ../boot/lock.txt
  echo $(id -un)":" >> ../boot/lock.txt
  echo "  locked "$(date) >> ../boot/lock.txt
  echo "    -- "$1 >> ../boot/lock.txt
  if svn commit ../boot/lock.txt -m "Set lock. $1"; then
    echo OK: set lock succesful;
  else
    echo FAIL: something went wrong;
  fi
else echo FAIL: project is already locked; fi

