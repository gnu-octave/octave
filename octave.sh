#!/bin/sh
#
# Wrapper for octave for binary installations that can't install
# octave in /usr/local/bin.
#
# The real binary should be installed in as octave.bin, and this file
# should be installed in the same directory as octave.

OCTAVE_HOME=@OCTAVE_HOME@
export OCTAVE_HOME

exec $OCTAVE_HOME/bin/octave.bin $*
