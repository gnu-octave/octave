function retval = hadamard (k)

# usage: hadamard (k)
#
# Return the Hadamard matrix of order n = 2^k.
#
# See also: hankel, vander, hilb, invhilb, toeplitz

  if (nargin != 1)
    error ("usage: hadamard (n)");
  endif

  if (k < 1)
    retval = 1;
  else
    tmp = hadamard (k-1);
    retval = [tmp, tmp; tmp, -tmp];
  endif

endfunction
