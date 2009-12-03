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
  if [ -f "$arg" ]; then
    file="$arg"
  else
    file="$srcdir/$arg"
  fi
  if [ "`egrep -l "$DEFUN_PATTERN" $file`" ]; then
    echo "$file" | sed 's,.*/,,; s/\.\(cc\|yy\|ll\)$/.df/';
  fi
done
