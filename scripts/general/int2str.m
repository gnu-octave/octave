function retval = int2str (x)

# usage: int2str (x)
#
# Round x to the nearest integer and format as a string.
#
# See also: sprintf, num2str 

  if (nargin == 1)
    if (rows (x) == 1 && columns (x) == 1)
      retval = sprintf ("%f\n", round (x));
    else
      error ("int2str: expecting scalar argument");
    endif
  else
    error ("usage: int2str (x)");
  endif

endfunction
