#include <octave/oct.h>
#include <octave/interpreter.h>

DEFMETHOD_DLD (globaldemo, interp, args, , "Global Demo")
{
  if (args.length () != 1)
    print_usage ();

  octave_value retval;

  std::string s = args(0).string_value ();

  octave_value tmp = interp.global_varval (s);

  if (tmp.is_defined ())
    retval = tmp;
  else
    retval = "Global variable not found";

  interp.global_assign ("a", 42.0);

  return retval;
}
