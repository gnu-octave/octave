function retval = invhilb (n)

# usage: invhilb (n)
#
# Return the inverse of a Hilbert matrix of order n.  This is slow but
# exact.  Compare with inv (hilb (n)).
#
# See also: hankel, vander, hadamard, hilb, toeplitz

  if (nargin != 1)
    error ("usage: invhilb (n)");
  endif

  nmax = length (n);
  if (nmax == 1)
    retval = zeros (n);
    for l = 1:n
      for k = l:n
        tmp = 1;
        for i = 1:n
          tmp = tmp * (i + k - 1);
        endfor
        for i = 1:n
          if (i != k)
            tmp = tmp * (l + i - 1);
          endif
        endfor
        for i = 1:n
          if (i != l)
            tmp = tmp / (i - l);
          endif
        endfor
        for i = 1:n
          if (i != k)
            tmp = tmp / (i - k);
          endif
        endfor
        retval (k, l) = tmp;
        retval (l, k) = tmp;
      endfor
    endfor
  else
    error ("hilb: expecting scalar argument, found something else");
  endif

endfunction
