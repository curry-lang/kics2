#!/bin/sh
# generate intermediate files of all libraries until everything is compiled

CURRY=kics2
FRONTEND=bin/$CURRY-frontend
FRONTENDPARAMS="--extended -ilib AllLibraries"

compile_all() {
  $FRONTEND --flat       $FRONTENDPARAMS
  $FRONTEND --typed-flat $FRONTENDPARAMS
  $FRONTEND --acy        $FRONTENDPARAMS
  $FRONTEND --uacy       $FRONTENDPARAMS
}

TMPOUT=TMPLIBOUT
CCODE=0

while [ $CCODE = 0 ] ; do
  compile_all | tee $TMPOUT
  echo NEW COMPILED LIBRARIES IN THIS ITERATION:
  grep Compiling $TMPOUT
  CCODE=$?
done
/bin/rm -r $TMPOUT
