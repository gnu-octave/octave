function x = kron (a, b)

# Usage: x = kron (a, b)
#
# Form the Kronecker product of two matrices, defined block by block
# as 
#
#   x = [a(i,j) b]

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin == 2)

    [m, n] = size (b);
    [ma, na] = size (a);

# Do 1st column.

    x = a (1, 1) * b;
    for ii = 2:ma
      x = [x; a (ii, 1)*b];
    endfor

# Do remaining columns.

    for jj = 2:na
      tmp = a (1, jj)*b;
      for ii = 2:ma
	tmp = [tmp; a (ii, jj)*b];
      endfor
      x = [x, tmp];
    endfor

  else
    error ("usage: kron (a, b)");
  endif

endfunction
