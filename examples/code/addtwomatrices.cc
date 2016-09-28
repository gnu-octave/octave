#include <octave/oct.h>

DEFUN_DLD (addtwomatrices, args, , "Add A to B")
{
  if (args.length () != 2)
    print_usage ();

  NDArray A = args(0).array_value ();
  NDArray B = args(1).array_value ();

  return octave_value (A + B);
}
