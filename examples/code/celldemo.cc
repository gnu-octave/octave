#include <octave/oct.h>
#include <octave/Cell.h>

DEFUN_DLD (celldemo, args, , "Cell Demo")
{
  octave_value_list retval;

  if (args.length () != 1)
    print_usage ();

  Cell c = args(0).cell_value ();

  for (octave_idx_type i = 0; i < c.numel (); i++)
    {
      retval(i) = c(i);          // using operator syntax
      //retval(i) = c.elem (i);  // using method syntax
    }

  return retval;
}
