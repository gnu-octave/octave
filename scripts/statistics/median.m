function retval = median (a)

# usage: median (a)
#
# For vector arguments, return the median of the values.
#
# For matrix arguments, return a row vector containing the median for
# each column.
#
# See also: std, mean

  if (nargin != 1)
    error ("usage: medain (a)");
  endif

  [nr, nc] = size (a);
  s = sort (a);
  if (nr == 1 && nc > 0)
    if (rem (nc, 2) == 0)
      i = nc/2;
      retval = (s (i) + s (i+1)) / 2;
    else
      i = ceil (nc/2);
      retval = s (i);
    endif
  elseif (nr > 0 && nc > 0)
    if (rem (nr, 2) == 0)
      i = nr/2;
      retval = (s (i,:) + s (i+1,:)) / 2;
    else
      i = ceil (nr/2);
      retval = s (i,:);
    endif
  else
    error ("median: invalid matrix argument");
  endif

endfunction
