#!/bin/bash

# Sometimes when changing quotation mechanism we need to make sure no two people modify boot binaries
# at the same time.

# TODO: This should use native svn locking on boot/.

cd ..
svn up
cd ncc
if test $(tail -n 1 ../boot/lock.txt | awk '{print $1 }';) = "unlocked"; then 
  cp ../boot/lock.txt locktmp.txt
  echo "" >> ../boot/lock.txt
  echo $(id -un)":" >> ../boot/lock.txt
  echo "  locked, "$(date) >> ../boot/lock.txt
  echo "    -- "$1 >> ../boot/lock.txt
  if svn commit ../boot/lock.txt -m "Set lock. $1"; then
    echo OK: set lock succesful;
    rm -f locktmp.txt;
  else
    echo FAIL: something went wrong;
    mv -f locktmp.txt ../boot/lock.txt;
  fi
else echo FAIL: project is already locked; fi

