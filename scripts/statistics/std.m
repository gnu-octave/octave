function retval = std (a)

# usage: std (a)
#
# For vector arguments, std returns the standard deviation of the
# values.  For matrix arguments, std returns a row vector containing
# the standard deviation for each column.
#
# See also: mean, median

  if (nargin != 1)
    error ("usage: std (a)");
  endif

  nr = rows (a);
  nc = columns (a);
  if (nc == 1 && nr == 1)
    retval = 0;
  elseif (nc == 1 || nr == 1)
    tmp = sum (a);
    n = length (a);
    retval = sqrt ((n * sumsq (a) - tmp .* tmp) / (n * (n - 1)));
  elseif (nr > 1 && nc > 0)
    tmp = sum (a);
    retval = sqrt ((nr * sumsq (a) - tmp .* tmp) / (nr * (nr - 1)));
  else
    error ("mean: invalid matrix argument");
  endif

endfunction
