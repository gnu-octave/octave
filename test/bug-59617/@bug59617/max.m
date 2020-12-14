function r = max (a, b)
  if (nargin != 2)
    error ("@bug59617/max: invalid number of arguments");
  endif
  if (! isa (a, "bug59617") && ! isa (b, "bug59617"))
    error ("@bug59617/max: expecting one argument to be a bug59617 object");
  endif
  r = "@bug59617/max";
endfunction
