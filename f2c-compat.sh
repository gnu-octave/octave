#!/bin/sh
#
# f2c-compat -- try to see if calling a Fortran compiled routine from
# a C compiled main program will work as if the Fortran routine has
# been translated to C via f2c.
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

# trap 'rm -f ftest* ctest* core; exit 1' 1 3 15

status=1

if test $# -eq 0; then
  FLIBS_SH="./flibs.sh"
elif test $# -eq 1; then
  FLIBS_SH="$1"
else
  echo "usage: f2c-compat.sh [flibs_script]"
  exit 1
fi

# Write a minimal program, compile it, and see if it works as
# expected.

cat << EOF > ftest.f
      INTEGER FUNCTION FORSUB (C, D)
      CHARACTER *(*) C
      INTEGER L
      DOUBLE PRECISION D
      L = LEN (C)
      WRITE (*, '(A,1X,I2)') C(1:L), INT (D)
      FORSUB = 1
      RETURN
      END
EOF
${F77-f77} -c ftest.f > /dev/null 2>&1

cat << EOF > ctest.c
extern int strlen ();
extern int strcpy ();
extern int forsub_ ();
static char s[14];
int
main (argc, argv)
  int argc;
  char **argv;
{
  double d = 10.0;
  int len;
  strcpy (s, "FOO-I-HITHERE");
  len = strlen (s);
  return (! forsub_ (s, &d, len));
}
/* For Sun f77 */
int
MAIN_ ()
{
  return 0;
}
EOF

${CC-cc} -c ctest.c > /dev/null 2>&1

FLIBS=`F77="${F77-f77}" $FLIBS_SH`

${CC-cc} -o ctest ctest.o ftest.o $FLIBS -lm > /dev/null 2>&1

ctest_output=`./ctest 2>&1`
status=$?

if test $status -eq 0 && test "$ctest_output" = "FOO-I-HITHERE 10"
then
  echo '-DF77_APPEND_UNDERSCORE=1'
  status=0
else
  cat << EOF > ctest.c
extern int strlen ();
extern int strcpy ();
extern int forsub ();
static char s[14];
int
main (argc, argv)
  int argc;
  char **argv;
{
  double d = 10.0;
  int len;
  strcpy (s, "FOO-I-HITHERE");
  len = strlen (s);
  return (! forsub (s, &d, len));
}
/* For Sun f77 */
int
MAIN_ ()
{
  return 0;
}
EOF

  ${CC-cc} -c ctest.c > /dev/null 2>&1

  ${CC-cc} -o ctest ctest.o ftest.o $FLIBS -lm > /dev/null 2>&1

  ctest_output=`./ctest 2>&1`
  status=$?

  if test $status -eq 0 && test "$ctest_output" = "FOO-I-HITHERE 10"
  then
    status=0
  fi
fi

rm -f ftest* ctest* core

# Bye-bye.

exit $status
