#! /bin/sh

set -e

# Some stupid egreps don't like empty elements in alternation patterns,
# so we have to repeat ourselves because some stupid egreps don't like
# empty elements in alternation patterns.

DEFUN_PATTERN="^[ \t]*DEF(CONSTFUN|CMD|UN|UN_DLD|UNX_DLD|UN_TEXT)[ \t]*\\("

srcdir="$1"
shift

for arg
do
  file=`echo "$arg" | sed 's/\.ll$/.cc/; s/\.yy$/.cc/'`;
  if [ ! -f $file ]; then
    file="$srcdir/$file"
  fi
  if [ "`egrep -l "$DEFUN_PATTERN" $file`" ]; then
    echo "$file" | sed 's/\.cc$/.df/';
  fi
done
