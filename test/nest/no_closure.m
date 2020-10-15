## no_closure.m
function r = no_closure (n)
  if (ischar (n))
    r = nested (n);
  else
    if (n == 0)
      r = @no_closure;
    elseif (n == 1)
      r = @nested;
    endif
  endif
  function r = nested (x)
    if (nargin == 1)
      r = ["nested ", x];
    else
      r = "nested";
    endif
  endfunction
endfunction
