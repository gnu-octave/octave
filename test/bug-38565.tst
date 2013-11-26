%!function r = f (x)
%!  if (ischar (x))
%!    r = x;
%!  else
%!    error ("expecting character string");
%!  endif
%!endfunction

%!assert (eval ("f 10;"), "10");
