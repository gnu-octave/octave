function y = fliplr (x)

# usage: fliplr (x)
#
# Return x with the columns swapped.
#
# See also: flipu, rot90

  if (nargin != 1)
    error ("usage: fliplr (x)");
  endif

  y = x;
  nc = columns (x);
  y = x (:, nc:-1:1);

endfunction
