#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ================
# Fcheck_script.sh
# ================
#
# --------------------------
# Check
# --------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fcheck_script.sh
#
#
# DESCRIPTION
# ===========
#
#
# Check if utilities are in the path, typically fcm.
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fcheck_script.sh fcm
#
#
# TODO
# ====
#
# option debug
#
#
# EVOLUTIONS
# ==========
#
# $Id$
#
#
#
#   * creation
#
#-

myscript=`which $1`
if [ ${#myscript} -eq 0 ]; then
echo "WARNING !!!"
echo "$1 has to be installed first"
echo "Exiting......................"
exit 1
fi

unset -v myscript
