#! /bin/sh
#
# Copyright (c) 2003-2005 The University of Wroclaw.
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

############################################################
# VARIABLES
############################################################

config_log="install.log"
mono_dir=
build_info=@build_info@
version=@version@


############################################################
# FUNCTIONS
############################################################

abort () {

    echo
    echo
    echo "*** $@ ***"
    if test -f $config_log; then
	echo "*** Check $config_log for details. ***"
    fi
    echo
    if [ "$ignore_errors" = yes ] ; then
      echo "*** Ignoring error. ***"
    else
      echo "Aborting..."
      rm -f $config_mak
      exit 1
    fi
}

echo_check_for () {

    echo "--------------- Checking for $@ ---------------" >> $config_log
    echo -n "Checking for $@... "
}


echo_check_if () {

    echo "--------------- Checking if $@ ----------------" >> $config_log
    echo -n "Checking if $@... "
}


echo_result () {

    echo "Result: $@" >> $config_log
    echo "-----------------------------------------------" >> $config_log
    echo >> $config_log
    echo "$@"
}


# This function tries to execute command given as an argument and returns
# shell exit code.  If the program doesn't exist in a path shell returns
# 127.  If the program is not a valid executable shell returns 126.
# If the program lacks some arguments it usually returns 1, otherwise
# this function should return 0.
#
# E.g.
# try_execute cc --version
# if test $? = 0; then
#    echo "found"
# else
#    echo "not found"
# fi
try_execute () {

    echo "Trying execute: $@" >> $config_log
    $@ >> $config_log 2>&1
    ret=$?
    echo "Execution status code: $ret." >> $config_log
    return $ret;
}


############################################################
# MAIN
############################################################

echo "starting installation, $version, $build_info" > $config_log
echo "current date: `date`" >> $config_log

cat <<EOF

                  *** Nemerle $version binary installer ***
		  
This script will install the Nemerle compiler $version.

You will need to have mono and gacutil binaries in your PATH.

In case this script fails you can contact any of:
  feedback@nemerle.org
  devel-en@nemerle.org (requires subscriptions)
  http://nemerle.org/bugs/ (Mantis bugtracker)
  http://nemerle.org/forum/ (phpBB forum)

Please attach $config_log.

Build info: $build_info

If you don't want to install Nemerle, please hit Ctrl-C now.  Otherwise
press Enter.

EOF
echo -n "Enter or Ctrl-C? "
read JUNK

echo_check_if "we're running from the right directory"
if test -f gac/Nemerle.Compiler.dll ; then
  echo_result "OK"
else
  echo_result "oops"
  abort "you need to 'cd' to directory where install-nemerle.sh is, before running it"
fi

echo_check_for "mono"
if try_execute mono --version ; then
  echo_result "OK"
else
  echo_result "FAIL!"
  abort "cannot run mono --version, please double check that running mono --version work from the command line"
fi

echo_check_for "gacutil"
try_execute gacutil
if [ $? -lt 100 ]; then
  echo_result "OK"
else
  echo_result "FAIL!"
  abort "cannot find gacutil, please double check that running gacutil from the command line gives the help message"
fi


echo_check_for "mono binary location"
oldIFS=$IFS
IFS=":"
set -- $PATH
IFS=$oldIFS

for dir ; do
  if test -x $dir/mono ; then
    mono_dir="$dir"
    break
  fi
done
if [ X"$mono_dir" != X ] ; then
  case $mono_dir in
    */ ) ;;
    * ) mono_dir="$mono_dir/" ;;
  esac
  echo_result $mono_dir
else
  echo_result "not found"
  echo "*** Warning, will use plain 'mono' without path in wrapper scripts ***"
fi

echo "Installing binaries to the GAC."
for f in gac/*.dll ; do
  gacutil -package nemerle -i $f || \
  abort "cannot install assembly to the GAC, please check the permissions"
done

  
echo "Creating nemerle.pc"

cat > nemerle.pc <<EOF
prefix=${prefix}
exec_prefix=\${prefix}
libdir=${libdir}

Name: Nemerle
Description: Nemerle - a .NET object-functional language
Version: $nem_version.$nem_revision
Libs: -r:${mono_libdir}/mono/nemerle/Nemerle.dll -r:${mono_libdir}/mono/nemerle/Nemerle.Compiler.dll -r:${mono_libdir}/mono/nemerle/Nemerle.Macros.dll
EOF

