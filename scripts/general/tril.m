function retval = tril (x, k)

# usage: triu (x, k)
#
# Return the lower triangular part of x above the k-th diagonal.  If
# the second argument is omitted, k = 0 is assumed.
#
# See also: triu, diag

  if (nargin > 0)
    [nr, nc] = size (x);
    retval = x;
  endif

  if (nargin == 1)
    k = 0;
  elseif (nargin == 2)
    max_nr_nc = max (nr, nc);
    if ((k > 0 && k > nr - 1) || (k < 0 && k < 1 - nc))
      error ("tril: requested diagonal out of range")
    endif
  else
    error ("usage: tril (x [, k])");
  endif

  for i = 1:nr
    for j = i+1-k:nc
      retval (i, j) = 0.0;
    endfor
  endfor

endfunction
