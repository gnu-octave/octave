function y = rot90 (x, k)

# usage: rot90 (x, k)
#
# Rotate the matrix x counterclockwise k*90 degrees.
#
# If the second argument is omitted, k is taken to be 1.
#
# See also: flipud, fliplr

  if (nargin < 2)
    k = 1;
  endif

  if (imag (k) != 0 || fix (k) != k)
    error ("rot90: k must be an integer");
  endif

  if (nargin == 1 || nargin == 2)
    k = rem (k, 4);
    if (k < 0)
      k = k + 4;
    endif
    if (k == 0)
      y = x;
    elseif (k == 1)
      y = flipud (x');
    elseif (k == 2)
      y = flipud (fliplr (x));
    elseif (k == 3)
      y = (flipud (x))';
    else
      error ("rot90: internal error!");
    endif
  else
    error ("usage: rot90 (x [, k])");
  endif

endfunction
