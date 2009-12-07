#include <octave/oct.h>
#include <octave/ov-struct.h>

DEFUN_DLD (structdemo, args, , "Struct demo.")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin != 2)
    print_usage ();
  else
    {
      Octave_map arg0 = args(0).map_value ();
      std::string arg1 = args(1).string_value ();

      if (! error_state && arg0.contains (arg1))
        {
          // The following two lines might be written as
          //    octave_value tmp;
          //    for (Octave_map::iterator p0 = 
          //        arg0.begin(); 
          //        p0 != arg0.end(); p0++ )
          //      if (arg0.key (p0) == arg1)
          //        {
          //          tmp = arg0.contents (p0) (0);
          //          break;
          //        }
          // though using seek is more concise.
          Octave_map::const_iterator p1 = arg0.seek (arg1);
          octave_value tmp =  arg0.contents(p1)(0);
          Octave_map st;
          st.assign ("selected", tmp);
          retval = octave_value (st);
        }
    }
  return retval; 
}

