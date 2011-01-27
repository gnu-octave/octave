#include <octave/oct.h>
#include <octave/unwind-prot.h>

void
err_hand (const char *fmt, ...)
{
  // Do nothing!!
}

DEFUN_DLD (unwinddemo, args, nargout, "Unwind Demo")
{
  int nargin = args.length();
  octave_value retval;
  if (nargin < 2)
    print_usage ();
  else
    {
      NDArray a = args(0).array_value ();
      NDArray b = args(1).array_value ();

      if (! error_state)
        {
          unwind_protect::begin_frame ("Funwinddemo");
          unwind_protect_ptr (current_liboctave_warning_handler);
          set_liboctave_warning_handler(err_hand);
          retval = octave_value (quotient (a, b));
          unwind_protect::run_frame ("Funwinddemo");
        }
    }
  return retval;
}
