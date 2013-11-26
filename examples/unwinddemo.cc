#include <octave/oct.h>
#include <octave/unwind-prot.h>

void
my_err_handler (const char *fmt, ...)
{
  // Do nothing!!
}

DEFUN_DLD (unwinddemo, args, nargout, "Unwind Demo")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();
  else
    {
      NDArray a = args(0).array_value ();
      NDArray b = args(1).array_value ();

      if (! error_state)
        {
          // Declare unwind_protect frame which lasts as long as
          // the variable frame has scope.
          unwind_protect frame;
          frame.protect_var (current_liboctave_warning_handler);

          set_liboctave_warning_handler (my_err_handler);
          retval = octave_value (quotient (a, b));
        }
    }
  return retval;
}
