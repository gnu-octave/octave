function x = varg_nest2
  [a, b] = f;
  x = a;

  if (nargout == 1)
    x = a;
  endif

  function [a, b] = f
    if (nargout == 2)
      a = b = 5;
    endif
  endfunction
endfunction
