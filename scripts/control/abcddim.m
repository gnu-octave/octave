function [n, m, p] = abcdchk (a, b, c, d)

# Usage: [n, m, p] = abcdchk (a, b, c, d)
#
# Check for compatibility of the dimensions of the matrices defining
# the linear system (a, b, c, d).
#
# returns n = number of system states, 
#         m = number of system inputs
#         p = number of system outputs
#
# returns n = m = p = -1 if system is not compatible

  if (nargin != 4)
    error ("abcdchk: illegal number of arguments.  Need four.")
  endif

  n = -1;
  m = -1;
  p = -1;

  [an, am] = size (a);
  if (an != am)
    disp ("abcdchk: a is not square");
    return;
  endif

  [bn, bm] = size (b);
  if (bn != am)
    disp ("abcdchk: a, b are not compatible");
    return;
  endif

  [cn, cm] = size (c);
  if (cm != an)
    disp ("abcdchk: a, c are not compatible");
    return;
  endif

  [dn, dm] = size (d);
  if (cn != dn)
    disp ("abcdchk: c, d are not compatible");
    return;
  endif

  if (bm != dm)
    disp ("abcdchk: b, d are not compatible");
    return;
  endif

  n = an;
  m = bm;
  p = cn;

endfunction
