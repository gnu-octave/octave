function retval = hilb (n)

# usage: hilb (n)
#
# Return the Hilbert matrix of order n.  The i, j element of a Hilbert
# matrix is defined as
#
#  H (i, j) = 1 / (i + j - 1);
#
# See also: hankel, vander, hadamard, invhilb, toeplitz


  if (nargin != 1)
    error ("usage: hilb (n)");
  endif

  nmax = length (n);
  if (nmax == 1)
    retval = zeros (n);
    for j = 1:n
      for i = 1:n
        retval (i, j) = 1 / (i + j - 1);
      endfor
    endfor
  else
    error ("hilb: expecting scalar argument, found something else");
  endif

endfunction
