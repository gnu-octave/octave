#include <octave/oct.h>
#include <octave/parse.h>

DEFMETHOD_DLD (funcdemo, interp, args, nargout, "Function Demo")
{
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();

  octave_value_list newargs;

  for (octave_idx_type i = nargin - 1; i > 0; i--)
    newargs(i-1) = args(i);

  octave_value_list retval;

  if (args(0).is_function_handle () || args(0).is_inline_function ()
      || args(0).is_string ())
    retval = interp.feval (args(0), newargs, nargout);
  else
    error ("funcdemo: INPUT must be string, inline, or function handle");

  return retval;
}
