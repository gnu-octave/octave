1;
function [...] = f (...)
  printf ("nargin: %d, nargout: %d\n", nargin, nargout);
  vr_val (va_arg ());
endfunction
z = f (1);
