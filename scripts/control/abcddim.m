function [n, m, p] = abcddim (a, b, c, d)

# Usage: [n, m, p] = abcddim (a, b, c, d)
#
# Check for compatibility of the dimensions of the matrices defining
# the linear system (a, b, c, d).
#
# Returns n = number of system states,
#         m = number of system inputs,
#         p = number of system outputs.
#
# Returns n = m = p = -1 if the system is not compatible.

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin != 4)
    error ("abcddim: illegal number of arguments.  need four.")
  endif

  n = m = p = -1;

  [an, am] = size(a);
  if (an != am)
    fprintf (stderr, "abcddim: a is not square");
    return;
  endif

  [bn, bm] = size(b);
  if (bn != am)
    fprintf (stderr, "abcddim: a and b are not compatible");
    return;
  endif

  [cn, cm] = size(c);
  if (cm != an)
    fprintf (stderr, "abcddim: a and c are not compatible");
    return;
  endif

  [dn, dm] = size(d);
  if (cn != dn)
    fprintf (stderr, "abcddim: c and d are not compatible");
    return;
  endif

  if (bm != dm)
    fprintf (stderr, "abcddim: b and d are not compatible");
    return;
  endif

  n = an;
  m = bm;
  p = cn;

endfunction
