#!/bin/sh

# Start interactive read-eval-print loop for KiCS2

# The installation directory of KiCS2
KICS2HOME=`echo KICS2HOME must be defined here!`

# check whether first argument is a tool and, if yes, exec the tool
KICS2TOOL="$KICS2HOME/bin/kics2-"$1
if [ -x "$KICS2TOOL" ] ; then
  shift
  exec "$KICS2TOOL" ${1+"$@"}
fi

# Add KiCS2 bin directory to path so that currypp can be found:
PATH=$PATH:$KICS2HOME/bin
export PATH

REPL="$KICS2HOME/bin/.local/kics2i"
if [ ! -x "$REPL" ] ; then
  echo "ERROR: executable '$REPL' not found!" >&2
  echo "Run: cd $KICS2HOME && make" >&2
  exit 1
fi

# use readline wrapper rlwrap if rlwrap exists, we have tty as stdin,
# and we have a home directory to store rlwrap's history:
USERLWRAP=no
if tty -s ; then
  RLWRAP=`which rlwrap`
  if [ -x "$RLWRAP" -a -d "$HOME" ] ; then
    USERLWRAP=yes
  fi
fi

if [ "$1" = "--noreadline" ] ; then
  shift
  USERLWRAP=no
fi

if [ $USERLWRAP = yes ] ; then
  exec rlwrap -c -f "$KICS2HOME/tools/rlwrap" "$REPL" ${1+"$@"}
else
  exec "$REPL" ${1+"$@"}
fi
