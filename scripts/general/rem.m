function retval = rem (x, y)

# usage: rem (x, y)
#
# Return remainder (x, y).

  if (nargin != 2)
    error ("usage: rem (x, y)");
  endif

  if (any (size (x) != size (y)))
    error ("rem: argument sizes must agree")
  endif

# Matlab allows complex arguments, but as far as I can tell, that's a
# bunch of hooey.

  if (any (any (imag (x))) || any (any (imag (y))))
    error ("rem: complex arguments are not allowed");
  endif

  if (nargin == 2)
    retval = x - y .* fix (x ./ y);
  else
    error ("usage: rem (x, y)");
  endif

endfunction
