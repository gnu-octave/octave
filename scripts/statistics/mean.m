function retval = mean (a)

# usage: mean (a)
#
# For vector arguments, return the mean the values.
#
# For matrix arguments, return a row vector containing the mean for
# each column.
#
# See also: median, std

  if (nargin != 1)
    error ("usage: mean (a)");
  endif

  [nr, nc] = size (a);
  if (nr == 1 || nc == 1)
    retval = sum (a) / length (a);
  elseif (nr > 0 && nc > 0)
    retval = sum (a) / nr;
  else
    error ("mean: invalid matrix argument");
  endif

endfunction
