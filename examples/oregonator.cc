#include <octave/oct.h>

#include <iostream.h>

DEFUN_DLD (oregonator, args, ,
  "The `oregonator'.")
{
  ColumnVector dx (3);

  ColumnVector x = args(0).vector_value ();

  dx(0) = 77.27 * (x(1) - x(0)*x(1) + x(0) - 8.375e-06*pow (x(0), 2));
  dx(1) = (x(2) - x(0)*x(1) - x(1)) / 77.27;
  dx(2) = 0.161*(x(0) - x(2));

  return octave_value (dx);
}
