function retval = triu (x, k)

# usage: triu (x, k)
#
# Return the upper triangular part of x above the k-th diagonal.  If
# the second argument is omitted, k = 0 is assumed.
#
# See also: tril, diag

  if (nargin > 0)
    [nr, nc] = size (x);
    retval = x;
  endif

  if (nargin == 1)
    k = 0;
  elseif (nargin == 2)
    max_nr_nc = max (nr, nc);
    if ((k > 0 && k > nc - 1) || (k < 0 && k < 1 - nr))
      error ("triu: requested diagonal out of range")
    endif
  else
    error ("usage: triu (x [, k])");
  endif

  for j = 1:nc
    for i = j+1-k:nr
      retval (i, j) = 0.0;
    endfor
  endfor

endfunction
