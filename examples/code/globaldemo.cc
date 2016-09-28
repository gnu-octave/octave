#include <octave/oct.h>

DEFUN_DLD (globaldemo, args, , "Global Demo")
{
  if (args.length () != 1)
    print_usage ();

  octave_value retval;

  std::string s = args(0).string_value ();

  octave_value tmp = get_global_value (s, true);

  if (tmp.is_defined ())
    retval = tmp;
  else
    retval = "Global variable not found";

  set_global_value ("a", 42.0);

  return retval;
}
