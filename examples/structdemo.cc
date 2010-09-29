#include <octave/oct.h>
#include <octave/ov-struct.h>

DEFUN_DLD (structdemo, args, , "Struct demo.")
{
  int nargin = args.length ();
  octave_value retval;

  if (args.length () == 2)
    {
      octave_scalar_map arg0 = args(0).scalar_map_value ();

      if (! error_state)
        {
          std::string arg1 = args(1).string_value ();

          if (! error_state)
            {
              octave_value tmp = arg0.contents (arg1);

              if (tmp.is_defined ())
                {
                  octave_scalar_map st;

                  st.assign ("selected", tmp);

                  retval = octave_value (st);
                }
              else
                error ("sruct does not contain field named '%s'\n",
                       arg1.c_str ());
            }
          else
            error ("expecting character string as second argument");
        }
      else
        error ("expecting struct as first argument");
    }
  else
    print_usage ();

  return retval; 
}

