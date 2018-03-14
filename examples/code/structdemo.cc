#include <octave/oct.h>
#include <octave/ov-struct.h>

DEFUN_DLD (structdemo, args, , "Struct Demo")
{
  if (args.length () != 2)
    print_usage ();

  if (! args(0).isstruct ())
    error ("structdemo: ARG1 must be a struct");

  octave_scalar_map arg0 = args(0).scalar_map_value ();
  //octave_map arg0 = args(0).map_value ();

  if (! args(1).is_string ())
    error ("structdemo: ARG2 must be a character string");

  std::string arg1 = args(1).string_value ();

  octave_value tmp = arg0.contents (arg1);
  //octave_value tmp = arg0.contents (arg1)(0);

  if (! tmp.is_defined ())
    error ("structdemo: struct does not have a field named '%s'\n",
           arg1.c_str ());

  octave_scalar_map st;

  st.assign ("selected", tmp);

  return octave_value (st);
}
