function retval = vander (c)

# usage: vander (c)
#
# Return the Vandermonde matrix whose next to last column is c.
#
# See also: hankel, hadamard, hilb, invhilb, toeplitz

  if (nargin != 1)
    error ("usage: vander (c)");
  endif

  nr = rows (c);
  nc = columns (c);
  if (nr == 1 && nc == 1)
    retval = 1;
  elseif (nr == 1 || nc == 1)
    n = length (c);
    if (n > 0)
      retval = zeros (n, n);
      for i = 1:n
        tmp = c(i);
        for j = 1:n
          retval (i, j) = tmp ^ (n - j);
        endfor
      endfor
    endif
  else
    error ("vander: argument must be a vector");
  endif

endfunction
