function y = flipud (x)

# usage: flipud (x)
#
# Return x with the rows swapped.
#
# See also: fliplr, rot90

  if (nargin != 1)
    error ("usage: flipud (x)");
  endif

  y = x;
  nr = rows (x);
  y = x (nr:-1:1, :);

endfunction
