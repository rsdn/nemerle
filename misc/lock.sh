#!/bin/bash
svn up ../boot/lock.txt
if test $(tail -n 1 ../boot/lock.txt | awk '{print $1 }';) = unlocked; then 
  echo "" >> lock.txt
  echo $(id -un)":" >> lock.txt
  echo "  locked "$(date) >> lock.txt
  echo "    -- "$1 >> lock.txt
  if svn commit ../boot/lock.txt -m "Set lock"; then
    echo OK: set lock succesful;
  else
    echo FAIL: something went wrong;
  fi
else echo FAIL: project is already locked; fi

