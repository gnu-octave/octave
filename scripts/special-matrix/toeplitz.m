function retval = toeplitz (c, r)

# usage: toeplitz (c, r)
#
# Return the Toeplitz matrix constructed given the first column
# c, and (optionally) the first row r.
#
# If the second argument is omitted, the first row is taken to be the
# same as the first column.  If the first element of c is not the same
# as the first element of r, the first element of c is used.
#
# See also: hankel, vander, hadamard, hilb, invhib

  if (nargin == 1)
    r = c;
  elseif (nargin != 2)
    error ("usage: toeplitz (c, r)");
  endif

  [c_nr, c_nc] = size (c);
  [r_nr, r_nc] = size (r);

  if ((c_nr != 1 && c_nc != 1) || (r_nr != 1 && r_nc != 1))
    error ("toeplitz: expecting vector arguments")
  endif

  if (c_nc != 1)
    c = c';
  endif

  if (r_nr != 1)
    r = r';
  endif

  if (r (1) != c (1))
    disp ("Column wins diagonal conflict");
  endif

# This should probably be done with the colon operator...

  nc = length (r);
  nr = length (c);

  retval = zeros (nr, nc);

  for i = 1:min (nc, nr)
    retval (i:nr, i) = c (1:nr-i+1);
  endfor

  for i = 1:min (nr, nc-1)
    retval (i, i+1:nc) = r (2:nc-i+1);
  endfor

endfunction
