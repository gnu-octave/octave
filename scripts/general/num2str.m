function retval = num2str (x)

# usage: num2str (x)
#
# Format x as a string.
#
# See also: sprintf, int2str

  if (nargin == 1)
    if (rows (x) == 1 && columns (x) == 1)
      retval = sprintf ("%g", x);
    else
      error ("num2str: expecting scalar argument");
    endif
  else
    error ("usage: num2str (x)");
  endif

endfunction
